module Atomo.Parser.Expand (doPragmas, macroExpand, nextPhase) where

import Control.Monad.State

import Atomo.Environment
import Atomo.Helpers
import Atomo.Method (addMethod, lookupMap)
import Atomo.Pattern (match)
import Atomo.Types


gensym :: Char
gensym = '!'

nextPhase :: [Expr] -> VM [Expr]
nextPhase es = do
    mapM_ doPragmas es
    mapM macroExpand es


doPragmas :: Expr -> VM ()
doPragmas (EDispatch { eMessage = em }) =
    pragmas em
  where
    pragmas (Single { mTarget = t }) =
        doPragmas t
    pragmas (Keyword { mTargets = ts }) =
        mapM_ doPragmas ts
doPragmas (EDefine { eExpr = e }) = do
    doPragmas e
doPragmas (ESet { eExpr = e }) = do
    doPragmas e
doPragmas (EBlock { eContents = es }) = do
    mapM_ doPragmas es
doPragmas (EList { eContents = es }) = do
    mapM_ doPragmas es
doPragmas (ETuple { eContents = es }) = do
    mapM_ doPragmas es
doPragmas (EMacro { emPattern = p, eExpr = e }) =
    addMacro p e
doPragmas (EParticle { eParticle = ep }) =
    case ep of
        Keyword { mTargets = mes } ->
            forM_ mes $ \me ->
                case me of
                    Nothing -> return ()
                    Just e -> doPragmas e

        Single { mTarget = Just e } ->
            doPragmas e

        _ -> return ()
doPragmas (EOperator {}) = return ()
doPragmas (EPrimitive {}) = return ()
doPragmas (EForMacro { eExpr = e }) = do
    env <- gets (psEnvironment . parserState)
    macroExpand e >>= withTop env . eval
    return ()
doPragmas (ETop {}) = return ()
doPragmas (EVM {}) = return ()
-- TODO: follow through EQuote into EUnquote
doPragmas (EQuote {}) = return ()
doPragmas (EUnquote {}) = return ()
doPragmas (ESetDynamic { eExpr = e }) =
    doPragmas e
doPragmas (EDefineDynamic { eExpr = e }) =
    doPragmas e
doPragmas (ENewDynamic { eBindings = bs, eExpr = e }) = do
    mapM_ (\(_, b) -> doPragmas b) bs
    doPragmas e
doPragmas (EGetDynamic {}) = return ()
doPragmas (EMacroQuote {}) = return ()


-- | Defines a macro, given its pattern and expression.
addMacro :: Message Pattern -> Expr -> VM ()
addMacro p e =
    modify $ \env -> env
        { parserState = (parserState env)
            { psMacros = withMacro (psMacros (parserState env))
            }
        }
  where
    withMacro ms =
        case p of
            Single {} ->
                (addMethod (Macro p e) (fst ms), snd ms)

            Keyword {} ->
                (fst ms, addMethod (Macro p e) (snd ms))


modifyPS :: (ParserState -> ParserState) -> VM ()
modifyPS f =
    modify $ \e -> e
        { parserState = f (parserState e)
        }

getPS :: VM ParserState
getPS = gets parserState

-- | Go through an expression recursively expanding macros. A dispatch
-- expression is checked to see if a macro was defined for it; if a macro is
-- found, its targets are sent to the macro method (unexpanded), and the
-- macro's result is expanded.
--
-- Every other expression just recursively calls macroExpand on any
-- sub-expressions.
macroExpand :: Expr -> VM Expr
macroExpand d@(EDispatch { eMessage = em }) = do
    mm <- findMacro msg
    case mm of
        Just m -> do
            modifyPS $ \ps -> ps { psClock = psClock ps + 1 }

            eb <- gensyms (mExpr m) >>= macroExpand
            Expression ne <-
                runMethod (m { mExpr = eb }) msg
                    >>= findExpression

            gensyms ne >>= macroExpand

        Nothing -> do
            nem <- expanded em
            return d { eMessage = nem }
  where
    expanded s@(Single { mTarget = t }) = do
        nt <- macroExpand t
        return s { mTarget = nt }
    expanded k@(Keyword { mTargets = ts }) = do
        nts <- mapM macroExpand ts
        return k { mTargets = nts }

    msg =
        case em of
            Single i n t os -> Single i n (Expression t) (map exprOpt os)
            Keyword i ns ts os -> Keyword i ns (map Expression ts) (map exprOpt os)

    exprOpt (Option i n e) = Option i n (Expression e)
macroExpand e@(EMacroQuote { eName = n, eRaw = r, eFlags = fs }) = do
    t <- gets (psEnvironment . parserState)
    liftM (EPrimitive (eLocation e)) . dispatch $
        keyword'
            ["quote", "as"]
            [t, string r, particle n]
            [option "flags" (list (map Character fs))]
macroExpand d@(EDefine { eExpr = e }) = do
    e' <- macroExpand e
    return d { eExpr = e' }
macroExpand s@(ESet { eExpr = e }) = do
    e' <- macroExpand e
    return s { eExpr = e' }
macroExpand b@(EBlock { eContents = es }) = do
    nes <- mapM macroExpand es
    return b { eContents = nes }
macroExpand l@(EList { eContents = es }) = do
    nes <- mapM macroExpand es
    return l { eContents = nes }
macroExpand t@(ETuple { eContents = es }) = do
    nes <- mapM macroExpand es
    return t { eContents = nes }
macroExpand p@(EParticle { eParticle = ep }) = do
    nos <- forM (mOptionals ep) $ \(Option i n me) -> do
        ne <- maybe (return Nothing) (liftM Just . macroExpand) me
        return (Option i n ne)

    case ep of
        Keyword { mNames = ns, mTargets = mes } -> do
            nmes <- forM mes $ \me ->
                case me of
                    Nothing -> return Nothing
                    Just e -> liftM Just (macroExpand e)

            return p { eParticle = keyword' ns nmes nos }

        Single { mName = n, mTarget = Just e } -> do
            ne <- macroExpand e
            return p { eParticle = single' n (Just ne) nos }

        _ -> return p
macroExpand s@(ESetDynamic { eExpr = e }) = do
    e' <- macroExpand e
    return s { eExpr = e' }
macroExpand d@(EDefineDynamic { eExpr = e }) = do
    e' <- macroExpand e
    return d { eExpr = e' }
macroExpand n@(ENewDynamic { eBindings = bs, eExpr = e }) = do
    bs' <- mapM (\(p, b) -> macroExpand b >>= \nb -> return (p, nb)) bs
    e' <- macroExpand e
    return n { eBindings = bs', eExpr = e' }
macroExpand m@(EMacro {}) = return m
macroExpand e@(EGetDynamic {}) = return e
macroExpand e@(EOperator {}) = return e
macroExpand e@(EPrimitive {}) = return e
macroExpand e@(EForMacro {}) = return e
macroExpand e@(ETop {}) = return e
macroExpand e@(EVM {}) = return e
macroExpand e@(EQuote {}) = expandQuote e
macroExpand e@(EUnquote {}) = return e

expandQuote :: Expr -> VM Expr
expandQuote = throughQuotes 0 $ \n e ->
    case n of
        0 -> macroExpand e
        _ -> return e

gensyms :: Expr -> VM Expr
gensyms = throughQuotes 0 $ \_ e ->
    case e of
        EDispatch { eMessage = m@(Single { mName = x:xs }) }
            | x == gensym -> do
                c <- gets (psClock . parserState)
                return e
                    { eMessage = single'
                        (xs ++ ":" ++ show c)
                        (mTarget m)
                        (mOptionals m)
                    }
        _ -> return e

throughQuotes :: Int -> (Int -> Expr -> VM Expr) -> Expr -> VM Expr
throughQuotes 0 f u@(EUnquote { eExpr = e }) = do
    ne <- f 0 e
    return u { eExpr = ne }
throughQuotes n f u@(EUnquote { eExpr = a }) = do
    ne <- throughQuotes (n - 1) f a
    f n u { eExpr = ne }
-- don't expand through definitions, as gensyms in those
-- are likely used elsewhere where they'll be expanded
throughQuotes n f d@(EDefine {}) = f n d
throughQuotes n f s@(ESet { ePattern = p, eExpr = e }) = do
    np <- expandPattern p
    ne <- throughQuotes n f e
    f n s { ePattern = np, eExpr = ne }
throughQuotes n f d@(EDispatch { eMessage = m@(Keyword {}) }) = do
    nts <- mapM (throughQuotes n f) (mTargets m)
    f n d { eMessage = m { mTargets = nts } }
throughQuotes n f d@(EDispatch { eMessage = m@(Single {}) }) = do
    nt <- throughQuotes n f (mTarget m)
    f n d { eMessage = m { mTarget = nt } }
throughQuotes n f b@(EBlock { eArguments = ps, eContents = es }) = do
    nps <- mapM expandPattern ps
    nes <- mapM (throughQuotes n f) es
    f n b { eArguments = nps, eContents = nes }
throughQuotes n f l@(EList { eContents = es }) = do
    nes <- mapM (throughQuotes n f) es
    f n l { eContents = nes }
throughQuotes n f t@(ETuple { eContents = es }) = do
    nes <- mapM (throughQuotes n f) es
    f n t { eContents = nes }
throughQuotes n f m@(EMacro { eExpr = e }) = do
    ne <- throughQuotes n f e
    f n m { eExpr = ne }
throughQuotes n f p@(EParticle { eParticle = m@(Keyword { mTargets = mes }) }) = do
    nmes <- forM mes $ maybe (return Nothing) (liftM Just . throughQuotes n f)
    f n p { eParticle = m { mTargets = nmes } }
throughQuotes n f p@(EParticle { eParticle = m@(Single { mTarget = me }) }) = do
    nme <- maybe (return Nothing) (liftM Just . throughQuotes n f) me
    f n p { eParticle = m { mTarget = nme } }
throughQuotes n f s@(ESetDynamic { eExpr = e }) = do
    e' <- throughQuotes n f e
    f n s { eExpr = e' }
throughQuotes n f d@(EDefineDynamic { eExpr = e }) = do
    e' <- throughQuotes n f e
    f n d { eExpr = e' }
throughQuotes n f d@(ENewDynamic { eBindings = bs, eExpr = e }) = do
    nbs <- mapM (\(p, b) -> f n b >>= \nb -> return (p, nb)) bs
    ne <- throughQuotes n f e
    f n d { eBindings = nbs, eExpr = ne }
throughQuotes n f q@(EQuote { eExpr = e }) = do
    ne <- throughQuotes (n + 1) f e
    f (n + 1) q { eExpr = ne }
throughQuotes n f e = f n e

expandPattern :: Pattern -> VM Pattern
expandPattern (PNamed n p)
    | head n == gensym = do
        c <- gets (psClock . parserState)
        np <- expandPattern p
        return (PNamed (tail n ++ ":" ++ show c) np)
    | otherwise = liftM (PNamed n) (expandPattern p)
expandPattern (PHeadTail h t) =
    liftM2 PHeadTail (expandPattern h) (expandPattern t)
expandPattern (PList ps) =
    liftM PList (mapM expandPattern ps)
expandPattern (PTuple ps) =
    liftM PTuple (mapM expandPattern ps)
expandPattern (PMessage (m@(Single { mTarget = t }))) = do
    nt <- expandPattern t
    return $ PMessage m { mTarget = nt }
expandPattern (PMessage (m@(Keyword { mTargets = ts }))) = do
    nts <- mapM expandPattern ts
    return $ PMessage m { mTargets = nts }
expandPattern (PInstance p) =
    liftM PInstance (expandPattern p)
expandPattern (PStrict p) =
    liftM PStrict (expandPattern p)
expandPattern (PVariable p) =
    liftM PVariable (expandPattern p)
expandPattern (PExpr p) =
    liftM PExpr (expandQuote p)
expandPattern (PPMKeyword ns ps) =
    liftM (PPMKeyword ns) (mapM expandPattern ps)
expandPattern p = return p

-- | find a findMacro method for message `m' on object `o'
findMacro :: Message Value -> VM (Maybe Method)
findMacro m = do
    ids <- gets primitives
    ms <- methods m
    return $ maybe Nothing (firstMatch ids m) (lookupMap (mID m) ms)
  where
    methods (Single {}) = liftM (fst . psMacros) getPS
    methods (Keyword {}) = liftM (snd . psMacros) getPS

    firstMatch _ _ [] = Nothing
    firstMatch ids' m' (mt:mts)
        | match ids' Nothing (PMessage (mPattern mt)) (Message m') =
            Just mt
        | otherwise = firstMatch ids' m' mts
