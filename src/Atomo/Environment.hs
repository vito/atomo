{-# LANGUAGE BangPatterns #-}
module Atomo.Environment where

import "monads-fd" Control.Monad.Cont
import "monads-fd" Control.Monad.State
import Data.IORef
import Data.List (nub)

import Atomo.Method
import Atomo.Pattern
import Atomo.Types


-- | Evaluate an expression, yielding a value.
eval :: Expr -> VM Value
eval (Define { ePattern = p, eExpr = ev }) = do
    define p ev
    return (particle "ok")
eval (Set { ePattern = p@(PSingle {}), eExpr = ev }) = do
    v <- eval ev
    define p (Primitive (eLocation ev) v)
    return v
eval (Set { ePattern = p@(PKeyword {}), eExpr = ev }) = do
    v <- eval ev
    define p (Primitive (eLocation ev) v)
    return v
eval (Set { ePattern = p, eExpr = ev }) = do
    v <- eval ev
    set p v
eval (Dispatch
        { eMessage = ESingle
            { emID = i
            , emName = n
            , emTarget = t
            }
        }) = do
    v <- eval t
    dispatch (Single i n v)
eval (Dispatch
        { eMessage = EKeyword
            { emID = i
            , emNames = ns
            , emTargets = ts
            }
        }) = do
    vs <- mapM eval ts
    dispatch (Keyword i ns vs)
eval (Operator { eNames = ns, eAssoc = a, ePrec = p }) = do
    forM_ ns $ \n -> modify $ \s ->
        s
            { parserState =
                (parserState s)
                    { psOperators =
                        (n, (a, p)) : psOperators (parserState s)
                    }
            }

    return (particle "ok")
eval (Primitive { eValue = v }) = return v
eval (EBlock { eArguments = as, eContents = es }) = do
    t <- gets top
    return (Block t as es)
eval (EList { eContents = es }) = do
    vs <- mapM eval es
    return (list vs)
eval (EMacro { ePattern = p, eExpr = e }) = do
    ps <- gets parserState
    modify $ \s -> s
        { parserState = ps
            { psMacros =
                case p of
                    PSingle {} ->
                        (addMethod (Macro p e) (fst (psMacros ps)), snd (psMacros ps))

                    PKeyword {} ->
                        (fst (psMacros ps), addMethod (Macro p e) (snd (psMacros ps)))

                    _ -> error $ "impossible: eval EMacro: p is " ++ show p
            }
        }

    return (particle "ok")
eval (EForMacro {}) = return (particle "ok")
eval (EParticle { eParticle = EPMSingle n }) =
    return (Particle $ PMSingle n)
eval (EParticle { eParticle = EPMKeyword ns mes }) = do
    mvs <- forM mes $
        maybe (return Nothing) (liftM Just . eval)
    return (Particle $ PMKeyword ns mvs)
eval (ETop {}) = gets top
eval (EVM { eAction = x }) = x
eval (EUnquote { eExpr = e }) = raise ["out-of-quote"] [Expression e]
eval (EQuote { eExpr = qe }) = do
    unquoted <- unquote 0 qe
    return (Expression unquoted)
  where
    unquote :: Int -> Expr -> VM Expr
    unquote 0 (EUnquote { eExpr = e }) = do
        r <- eval e
        case r of
            Expression e' -> return e'
            _ -> return (Primitive Nothing r)
    unquote n u@(EUnquote { eExpr = e }) = do
        ne <- unquote (n - 1) e
        return (u { eExpr = ne })
    unquote n d@(Define { eExpr = e }) = do
        ne <- unquote n e
        return (d { eExpr = ne })
    unquote n s@(Set { eExpr = e }) = do
        ne <- unquote n e
        return (s { eExpr = ne })
    unquote n d@(Dispatch { eMessage = em }) =
        case em of
            EKeyword { emTargets = ts } -> do
                nts <- mapM (unquote n) ts
                return d { eMessage = em { emTargets = nts } }

            ESingle { emTarget = t } -> do
                nt <- unquote n t
                return d { eMessage = em { emTarget = nt } }
    unquote n b@(EBlock { eContents = es }) = do
        nes <- mapM (unquote n) es
        return b { eContents = nes }
    unquote n l@(EList { eContents = es }) = do
        nes <- mapM (unquote n) es
        return l { eContents = nes }
    unquote n m@(EMacro { eExpr = e }) = do
        ne <- unquote n e
        return m { eExpr = ne }
    unquote n p@(EParticle { eParticle = ep }) =
        case ep of
            EPMKeyword ns mes -> do
                nmes <- forM mes $ \me ->
                    case me of
                        Nothing -> return Nothing
                        Just e -> liftM Just (unquote n e)

                return p { eParticle = EPMKeyword ns nmes }

            _ -> return p
    unquote n q@(EQuote { eExpr = e }) = do
        ne <- unquote (n + 1) e
        return q { eExpr = ne }
    unquote n p@(Primitive { eValue = Expression e }) = do
        ne <- unquote n e
        return p { eValue = Expression ne }
    unquote _ p@(Primitive {}) = return p
    unquote _ t@(ETop {}) = return t
    unquote _ v@(EVM {}) = return v
    unquote _ o@(Operator {}) = return o
    unquote _ f@(EForMacro {}) = return f

-- | Evaluate multiple expressions, returning the last result.
evalAll :: [Expr] -> VM Value
evalAll [] = throwError NoExpressions
evalAll [e] = eval e
evalAll (e:es) = eval e >> evalAll es

-- | Create a new empty object, passing a function to initialize it.
newObject :: (Object -> Object) -> VM Value
newObject f = liftM Reference . liftIO $
    newIORef . f $ Object
        { oDelegates = []
        , oMethods = noMethods
        }

-- | Run x with t as its toplevel object.
withTop :: Value -> VM a -> VM a
withTop t x = do
    o <- gets top
    modify (\e -> e { top = t })

    res <- x

    modify (\e -> e { top = o })

    return res


-----------------------------------------------------------------------------
-- Define -------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Insert a method on a single value.
defineOn :: Value -> Method -> VM ()
defineOn v m' = do
    o <- orefFor v
    obj <- liftIO (readIORef o)

    let (oss, oks) = oMethods obj
        ms (PSingle {}) = (addMethod m oss, oks)
        ms (PKeyword {}) = (oss, addMethod m oks)
        ms x = error $ "impossible: defining with pattern " ++ show x

    liftIO . writeIORef o $
        obj { oMethods = ms (mPattern m) }
  where
    m = m' { mPattern = setSelf v (mPattern m') }

-- | Define a method on all roles involved in its pattern.
define :: Pattern -> Expr -> VM ()
define !p !e = do
    is <- gets primitives
    newp <- matchable p
    m <- method newp e

    os <-
        case p of
            PKeyword { ppTargets = (t:_) } | isTop t ->
                targets is (head (ppTargets newp))

            _ -> targets is newp

    forM_ os $ \o ->
        defineOn (Reference o) m
  where
    isTop PThis = True
    isTop (PObject ETop {}) = True
    isTop _ = False

    method p' (Primitive _ v) = return (Slot p' v)
    method p' e' = gets top >>= \t -> return (Responder p' t e')


-- | Swap out a reference match with PThis, for inserting on an object.
setSelf :: Value -> Pattern -> Pattern
setSelf v (PKeyword i ns ps) =
    PKeyword i ns (map (setSelf v) ps)
setSelf v (PMatch x) | v == x = PThis
setSelf v (PNamed n p') =
    PNamed n (setSelf v p')
setSelf v (PSingle i n t) =
    PSingle i n (setSelf v t)
setSelf v (PInstance p) = PInstance (setSelf v p)
setSelf v (PStrict p) = PStrict (setSelf v p)
setSelf _ p' = p'


-- | Pattern-match a value, inserting bindings into the current toplevel.
set :: Pattern -> Value -> VM Value
set p v = do
    is <- gets primitives
    if match is Nothing p v
        then do
            forM_ (bindings' p v) $ \(p', v') ->
                define p' (Primitive Nothing v')

            return v
        else throwError (Mismatch p v)


-- | Turn any PObject patterns into PMatches.
matchable :: Pattern -> VM Pattern
matchable p'@(PSingle { ppTarget = t }) = do
    t' <- matchable t
    return p' { ppTarget = t' }
matchable p'@(PKeyword { ppTargets = ts }) = do
    ts' <- mapM matchable ts
    return p' { ppTargets = ts' }
matchable PThis = liftM PMatch (gets top)
matchable (PObject oe) = liftM PMatch (eval oe)
matchable (PInstance p) = liftM PInstance (matchable p)
matchable (PStrict p) = liftM PStrict (matchable p)
matchable (PNamed n p') = liftM (PNamed n) (matchable p')
matchable p' = return p'


-- | Find the target objects for a pattern.
targets :: IDs -> Pattern -> VM [ORef]
targets _ (PMatch v) = liftM (: []) (orefFor v)
targets is (PSingle _ _ p) = targets is p
targets is (PKeyword _ _ ps) = do
    ts <- mapM (targets is) ps
    return (nub (concat ts))
targets is (PNamed _ p) = targets is p
targets is PAny = return [idObject is]
targets is (PList _) = return [idList is]
targets is (PHeadTail h t) = do
    ht <- targets is h
    tt <- targets is t
    if idChar is `elem` ht || idString is `elem` tt
        then return [idList is, idString is]
        else return [idList is]
targets is (PPMKeyword {}) = return [idParticle is]
targets is (PExpr _) = return [idExpression is]
targets is (PInstance p) = targets is p
targets is (PStrict p) = targets is p
targets _ p = error $ "no targets for " ++ show p



-----------------------------------------------------------------------------
-- Dispatch -----------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Dispatch a message to all roles and return a value.
--
-- If the message is not understood, @did-not-understand:(at:) is sent to all
-- roles until one responds to it. If none of them handle it, a
-- @did-not-understand: error is raised.
dispatch :: Message -> VM Value
dispatch !m = do
    find <- findFirstMethod m (vs m)
    case find of
        Just method -> runMethod method m
        Nothing ->
            case vs m of
                [v] -> sendDNU v
                _ -> sendDNUs (vs m) 0
  where
    vs (Single { mTarget = t }) = [t]
    vs (Keyword { mTargets = ts }) = ts

    sendDNU v = do
        find <- findMethod v (dnuSingle v)
        case find of
            Nothing -> throwError $ DidNotUnderstand m
            Just method -> runMethod method (dnuSingle v)

    sendDNUs [] _ = throwError $ DidNotUnderstand m
    sendDNUs (v:vs') n = do
        find <- findMethod v (dnu v n)
        case find of
            Nothing -> sendDNUs vs' (n + 1)
            Just method -> runMethod method (dnu v n)

    dnu v n = keyword
        ["did-not-understand", "at"]
        [v, Message m, Integer n]

    dnuSingle v = keyword
        ["did-not-understand"]
        [v, Message m]


-- | Find a method on object `o' that responds to `m', searching its
-- delegates if necessary.
findMethod :: Value -> Message -> VM (Maybe Method)
findMethod v m = do
    is <- gets primitives
    r <- orefFor v
    o <- liftIO (readIORef r)
    case relevant is r o m of
        Nothing -> findFirstMethod m (oDelegates o)
        mt -> return mt

-- | Find the first value that has a method defiend for `m'.
findFirstMethod :: Message -> [Value] -> VM (Maybe Method)
findFirstMethod _ [] = return Nothing
findFirstMethod m (v:vs) = do
    r <- findMethod v m
    case r of
        Nothing -> findFirstMethod m vs
        _ -> return r

-- | Find a relevant method for message `m' on object `o'.
relevant :: IDs -> ORef -> Object -> Message -> Maybe Method
relevant ids r o m =
    lookupMap (mID m) (methods m) >>= firstMatch ids (Just r) m
  where
    methods (Single {}) = fst (oMethods o)
    methods (Keyword {}) = snd (oMethods o)

    firstMatch _ _ _ [] = Nothing
    firstMatch ids' r' m' (mt:mts)
        | match ids' r' (mPattern mt) (Message m') = Just mt
        | otherwise = firstMatch ids' r' m' mts

-- | Evaluate a method.
--
-- Responder methods: evaluates its expression in a scope with the pattern's
-- bindings, delegating to the method's context.
--
-- Slot methods: simply returns the value.
--
-- Macro methods: evaluates its expression in a scope with the pattern's
-- bindings.
runMethod :: Method -> Message -> VM Value
runMethod (Slot { mValue = v }) _ = return v
runMethod (Responder { mPattern = p, mContext = c, mExpr = e }) m = do
    nt <- newObject $ \o -> o
        { oDelegates = [c]
        , oMethods =
            ( bindings p m
            , emptyMap
            )
        }

    withTop nt $ eval e
runMethod (Macro { mPattern = p, mExpr = e }) m = do
    t <- gets top
    nt <- newObject $ \o -> o
        { oDelegates = [t]
        , oMethods = (bindings p m, emptyMap)
        }

    withTop nt $ eval e

-- | Execute an action with a new toplevel delegating to the old one.
newScope :: VM a -> VM a
newScope x = do
    t <- gets top
    nt <- newObject $ \o -> o
        { oDelegates = [t]
        }

    withTop nt x

-- | Get the object reference for a value.
orefFor :: Value -> VM ORef
{-# INLINE orefFor #-}
orefFor !v = gets primitives >>= \is -> return $ orefFrom is v

-- | Raise a keyword particle as an error.
raise :: [String] -> [Value] -> VM a
{-# INLINE raise #-}
raise ns vs = throwError . Error $ keyParticleN ns vs

-- | Raise a single particle as an error.
raise' :: String -> VM a
{-# INLINE raise' #-}
raise' = throwError . Error . particle

-- | Convert an AtomoError into a value and raise it as an error.
throwError :: AtomoError -> VM a
throwError e = gets top >>= \t -> do
    r <- dispatch (keyword ["responds-to?"] [t, particle "Error"])

    if r == Boolean True
        then do
            dispatch (msg t)
            error ("panic: error returned normally for: " ++ show e)
        else error ("panic: " ++ show e)
  where
    msg t = keyword ["error"] [t, asValue e]

