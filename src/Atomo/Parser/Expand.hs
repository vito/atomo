module Atomo.Parser.Expand (doPragmas, macroExpand, nextPhase) where

import Control.Monad.State
import Text.Parsec

import Atomo.Environment
import Atomo.Helpers
import Atomo.Method (addMethod, lookupMap)
import Atomo.Parser.Base
import Atomo.Pattern (match)
import Atomo.Types


nextPhase :: [Expr] -> Parser [Expr]
nextPhase es = do
    mapM_ doPragmas es
    ps <- getState
    lift $ modify $ \e -> e { parserState = ps }
    mapM macroExpand es


doPragmas :: Expr -> Parser ()
doPragmas (Dispatch { eMessage = em }) =
    pragmas em
  where
    pragmas (Single _ _ t) =
        doPragmas t
    pragmas (Keyword _ _ ts) =
        mapM_ doPragmas ts
doPragmas (Define { eExpr = e }) = do
    doPragmas e
doPragmas (Set { eExpr = e }) = do
    doPragmas e
doPragmas (EBlock { eContents = es }) = do
    mapM_ doPragmas es
doPragmas (EList { eContents = es }) = do
    mapM_ doPragmas es
doPragmas (EMacro { emPattern = p, eExpr = e }) = do
    {-e' <- doPragmas e-}
    macroExpand e >>= addMacro p
doPragmas (EParticle { eParticle = ep }) =
    case ep of
        PMKeyword _ mes ->
            forM_ mes $ \me ->
                case me of
                    Nothing -> return ()
                    Just e -> doPragmas e

        _ -> return ()
doPragmas (Operator {}) = return ()
doPragmas (Primitive {}) = return ()
doPragmas (EForMacro { eExpr = e }) = do
    env <- fmap psEnvironment getState
    macroExpand e >>= lift . withTop env . eval
    return ()
doPragmas (ETop {}) = return ()
doPragmas (EVM {}) = return ()
-- TODO: follow through EQuote into EUnquote
doPragmas (EQuote {}) = return ()
doPragmas (EUnquote {}) = return ()


-- | Defines a macro, given its pattern and expression.
addMacro :: Message Pattern -> Expr -> Parser ()
addMacro p e = do
    ms <- fmap psMacros getState
    modifyState $ \ps -> ps
        { psMacros = withMacro (psMacros ps)
        }
  where
    withMacro ms =
        case p of
            Single {} ->
                ( addMethod (Macro p e) (fst ms)
                , snd ms
                )

            Keyword {} ->
                ( fst ms
                , addMethod (Macro p e) (snd ms)
                )


-- | Go through an expression recursively expanding macros. A dispatch
-- expression is checked to see if a macro was defined for it; if a macro is
-- found, its targets are sent to the macro method (unexpanded), and the
-- macro's result is expanded.
--
-- Every other expression just recursively calls macroExpand on any
-- sub-expressions.
macroExpand :: Expr -> Parser Expr
macroExpand d@(Dispatch { eMessage = em }) = do
    mm <- findMacro msg
    case mm of
        Just m -> do
            modifyState $ \ps -> ps { psClock = psClock ps + 1 }

            ps <- getState

            Expression e <- lift $ do
                modify $ \e -> e { parserState = ps }
                runMethod m msg >>= findExpression

            macroExpand e

        Nothing -> do
            nem <- expanded em
            return d { eMessage = nem }
  where
    expanded (Single i n t) = do
        nt <- macroExpand t
        return (Single i n nt)
    expanded (Keyword i ns ts) = do
        nts <- mapM macroExpand ts
        return (Keyword i ns nts)

    msg =
        case em of
            Single i n t -> Single i n (Expression t)
            Keyword i ns ts -> Keyword i ns (map Expression ts)
macroExpand d@(Define { eExpr = e }) = do
    e' <- macroExpand e
    return d { eExpr = e' }
macroExpand s@(Set { eExpr = e }) = do
    e' <- macroExpand e
    return s { eExpr = e' }
macroExpand b@(EBlock { eContents = es }) = do
    nes <- mapM macroExpand es
    return b { eContents = nes }
macroExpand l@(EList { eContents = es }) = do
    nes <- mapM macroExpand es
    return l { eContents = nes }
macroExpand m@(EMacro { eExpr = e }) = do -- TODO: is this sane?
    e' <- macroExpand e
    return m { eExpr = e' }
macroExpand p@(EParticle { eParticle = ep }) =
    case ep of
        PMKeyword ns mes -> do
            nmes <- forM mes $ \me ->
                case me of
                    Nothing -> return Nothing
                    Just e -> liftM Just (macroExpand e)

            return p { eParticle = PMKeyword ns nmes }

        _ -> return p
macroExpand e@(Operator {}) = return e
macroExpand e@(Primitive {}) = return e
macroExpand e@(EForMacro {}) = return e
macroExpand e@(ETop {}) = return e
macroExpand e@(EVM {}) = return e
-- TODO: follow through EQuote into EUnquote
macroExpand e@(EQuote {}) = return e
macroExpand e@(EUnquote {}) = return e

-- | find a findMacro method for message `m' on object `o'
findMacro :: Message Value -> Parser (Maybe Method)
findMacro m = do
    ids <- lift (gets primitives)
    ms <- methods m
    maybe (return Nothing) (firstMatch ids m) (lookupMap (mID m) ms)
  where
    methods (Single {}) = liftM (fst . psMacros) getState
    methods (Keyword {}) = liftM (snd . psMacros) getState

    firstMatch _ _ [] = return Nothing
    firstMatch ids' m' (mt:mts)
        | match ids' Nothing (PMessage (mPattern mt)) (Message m') = return (Just mt)
        | otherwise = firstMatch ids' m' mts
