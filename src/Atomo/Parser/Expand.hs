module Atomo.Parser.Expand (macroExpand) where

import Control.Monad.State
import Text.Parsec
import qualified "mtl" Control.Monad.Trans as MTL

import Atomo.Environment
import Atomo.Helpers
import Atomo.Method (lookupMap)
import Atomo.Parser.Base
import Atomo.Pattern (match)
import Atomo.Types


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

            Expression e <-
                MTL.lift (runMethod m msg >>= findExpression)

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
-- TODO: EUnquote?
macroExpand e = return e

-- | find a findMacro method for message `m' on object `o'
findMacro :: Message Value -> Parser (Maybe Method)
findMacro m = do
    ids <- MTL.lift (gets primitives)
    ms <- methods m
    maybe (return Nothing) (firstMatch ids m) (lookupMap (mID m) ms)
  where
    methods (Single {}) = liftM (fst . psMacros) getState
    methods (Keyword {}) = liftM (snd . psMacros) getState

    firstMatch _ _ [] = return Nothing
    firstMatch ids' m' (mt:mts)
        | match ids' Nothing (PMessage (mPattern mt)) (Message m') = return (Just mt)
        | otherwise = firstMatch ids' m' mts
