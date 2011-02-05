{-# LANGUAGE BangPatterns #-}
module Atomo.Environment where

import Control.Monad.Cont
import Control.Monad.State
import Data.IORef

import Atomo.Method
import Atomo.Pattern
import Atomo.Pretty
import Atomo.Types


-- | Evaluate an expression, yielding a value.
eval :: Expr -> VM Value
eval (EDefine { emPattern = p, eExpr = ev }) = do
    define p ev
    return (particle "ok")
eval (ESet { ePattern = PMessage p, eExpr = ev }) = do
    v <- eval ev
    define p (EPrimitive (eLocation ev) v)
    return v
eval (ESet { ePattern = p, eExpr = ev }) = do
    v <- eval ev
    set p v
eval (EDispatch { eMessage = Single i n t [] }) = do
    v <- eval t
    dispatch (Single i n v [])
eval (EDispatch { eMessage = Single i n t os }) = do
    v <- eval t

    nos <- forM os $ \(Option oi on oe) -> do
        ov <- eval oe
        return (Option oi on ov)

    dispatch (Single i n v nos)
eval (EDispatch { eMessage = Keyword i ns ts [] }) = do
    vs <- mapM eval ts
    dispatch (Keyword i ns vs [])
eval (EDispatch { eMessage = Keyword i ns ts os }) = do
    vs <- mapM eval ts

    nos <- forM os $ \(Option oi on e) -> do
        ov <- eval e
        return (Option oi on ov)

    dispatch (Keyword i ns vs nos)
eval (EOperator { eNames = ns, eAssoc = a, ePrec = p }) = do
    forM_ ns $ \n -> modify $ \s ->
        s
            { parserState =
                (parserState s)
                    { psOperators =
                        (n, (a, p)) : psOperators (parserState s)
                    }
            }

    return (particle "ok")
eval (EPrimitive { eValue = v }) = return v
eval (EBlock { eArguments = as, eContents = es }) = do
    t <- gets top
    return (Block t as es)
eval (EList { eContents = es }) = do
    vs <- mapM eval es
    return (list vs)
eval (ETuple { eContents = es }) = do
    vs <- mapM eval es
    return (tuple vs)
eval (EMacro {}) = return (particle "ok")
eval (EForMacro {}) = return (particle "ok")
eval (EParticle { eParticle = Single i n mt os }) = do
    nos <- forM os $ \(Option oi on me) ->
        liftM (Option oi on)
            (maybe (return Nothing) (liftM Just . eval) me)

    nmt <- maybe (return Nothing) (liftM Just . eval) mt

    return (Particle $ Single i n nmt nos)
eval (EParticle { eParticle = Keyword i ns mts os }) = do
    nmts <- forM mts $
        maybe (return Nothing) (liftM Just . eval)

    nos <- forM os $ \(Option oi on me) ->
        liftM (Option oi on)
            (maybe (return Nothing) (liftM Just . eval) me)

    return (Particle $ Keyword i ns nmts nos)
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
            _ -> return (EPrimitive Nothing r)
    unquote n u@(EUnquote { eExpr = e }) = do
        ne <- unquote (n - 1) e
        return (u { eExpr = ne })
    unquote n q@(EQuote { eExpr = e }) = do
        ne <- unquote (n + 1) e
        return q { eExpr = ne }
    unquote n d@(EDefine { eExpr = e }) = do
        ne <- unquote n e
        return (d { eExpr = ne })
    unquote n s@(ESet { eExpr = e }) = do
        ne <- unquote n e
        return (s { eExpr = ne })
    unquote n d@(EDispatch { eMessage = em }) =
        case em of
            Keyword { mTargets = ts } -> do
                nts <- mapM (unquote n) ts
                return d { eMessage = em { mTargets = nts } }

            Single { mTarget = t } -> do
                nt <- unquote n t
                return d { eMessage = em { mTarget = nt } }
    unquote n b@(EBlock { eContents = es }) = do
        nes <- mapM (unquote n) es
        return b { eContents = nes }
    unquote n l@(EList { eContents = es }) = do
        nes <- mapM (unquote n) es
        return l { eContents = nes }
    unquote n t@(ETuple { eContents = es }) = do
        nes <- mapM (unquote n) es
        return t { eContents = nes }
    unquote n m@(EMacro { eExpr = e }) = do
        ne <- unquote n e
        return m { eExpr = ne }
    unquote n p@(EParticle { eParticle = ep }) =
        case ep of
            Keyword { mNames = ns, mTargets = mes } -> do
                nmes <- forM mes $ \me ->
                    case me of
                        Nothing -> return Nothing
                        Just e -> liftM Just (unquote n e)

                return p { eParticle = keyword ns nmes }

            _ -> return p
    unquote n d@(ENewDynamic { eExpr = e }) = do
        ne <- unquote n e
        return d { eExpr = ne }
    unquote n d@(EDefineDynamic { eExpr = e }) = do
        ne <- unquote n e
        return d { eExpr = ne }
    unquote n d@(ESetDynamic { eExpr = e }) = do
        ne <- unquote n e
        return d { eExpr = ne }
    unquote n p@(EPrimitive { eValue = Expression e }) = do
        ne <- unquote n e
        return p { eValue = Expression ne }
    unquote n m@(EMatch { eTarget = t, eBranches = bs }) = do
        nt <- unquote n t
        nbs <- forM bs $ \(p, e) -> do
            ne <- unquote n e
            return (p, ne)
        return m { eTarget = nt, eBranches = nbs }
    unquote _ p@(EPrimitive {}) = return p
    unquote _ t@(ETop {}) = return t
    unquote _ v@(EVM {}) = return v
    unquote _ o@(EOperator {}) = return o
    unquote _ f@(EForMacro {}) = return f
    unquote _ g@(EGetDynamic {}) = return g
    unquote _ q@(EMacroQuote {}) = return q
eval (ENewDynamic { eBindings = bes, eExpr = e }) = do
    bvs <- forM bes $ \(n, b) -> do
        v <- eval b
        return (n, v)

    dynamicBind bvs (eval e)
eval (EDefineDynamic { eName = n, eExpr = e }) = do
    v <- eval e

    modify $ \env -> env
        { dynamic = newDynamic n v (dynamic env)
        }

    return v
eval (ESetDynamic { eName = n, eExpr = e }) = do
    v <- eval e
    d <- gets dynamic

    if isBound n d
        then modify $ \env -> env { dynamic = setDynamic n v d }
        else raise ["unbound-dynamic"] [string n]

    return v
eval (EGetDynamic { eName = n }) = do
    mv <- gets (getDynamic n . dynamic)
    maybe (raise ["unknown-dynamic"] [string n]) return mv
eval (EMacroQuote { eName = n, eRaw = r, eFlags = fs }) = do
    t <- gets (psEnvironment . parserState)
    dispatch $
        keyword'
            ["quote", "as"]
            [t, string r, particle n]
            [option "flags" (list (map Character fs))]
eval (EMatch { eTarget = t, eBranches = bs }) = do
    v <- eval t
    ids <- gets primitives
    matchBranches ids bs v

matchBranches :: IDs -> [(Pattern, Expr)] -> Value -> VM Value
matchBranches _ [] v = raise ["no-match-for"] [v]
matchBranches ids ((p, e):ps) v = do
    p' <- matchable' p
    if match ids Nothing p' v
        then newScope $ set p' v >> eval e
        else matchBranches ids ps v

-- | Evaluate multiple expressions, returning the last result.
evalAll :: [Expr] -> VM Value
evalAll [] = throwError NoExpressions
evalAll [e] = eval e
evalAll (e:es) = eval e >> evalAll es

-- | Create a new empty object, passing a function to initialize it.
newObject :: Delegates -> (MethodMap, MethodMap) -> VM Value
newObject ds mm = do
    ms <- liftIO (newIORef mm)
    return (Object ds ms)

-- | Run x with t as its toplevel object.
withTop :: Value -> VM a -> VM a
withTop !t x = do
    o <- gets top
    modify (\e -> e { top = t })

    res <- x

    modify (\e -> e { top = o })

    return res

-- | Execute an action with the given dynamic bindings.
dynamicBind :: [(String, Value)] -> VM a -> VM a
dynamicBind bs x = do
    modify $ \e -> e
        { dynamic = foldl (\m (n, v) -> bindDynamic n v m) (dynamic e) bs
        }

    res <- x

    modify $ \e -> e
        { dynamic = foldl (\m (n, _) -> unbindDynamic n m) (dynamic e) bs
        }

    return res

-- | Execute an action with a new toplevel delegating to the old one.
newScope :: VM a -> VM a
newScope x = do
    t <- gets top
    nt <- newObject [t] noMethods
    withTop nt x


-----------------------------------------------------------------------------
-- Define -------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Insert a method on a single value.
defineOn :: Value -> Method -> VM ()
defineOn v m' = liftIO $ do
    (oss, oks) <- readIORef (oMethods v)

    writeIORef (oMethods v) $
        case mPattern m of
            Single {} ->
                (addMethod m oss, oks)

            Keyword {} ->
                (oss, addMethod m oks)
  where
    m = m' { mPattern = setSelf v (mPattern m') }

-- | Define a method on all roles involved in its pattern.
define :: Message Pattern -> Expr -> VM ()
define !p !e = do
    is <- gets primitives
    newp <- matchable p
    m <- method newp e
    os <- targets is newp
    forM_ os (flip defineOn m)
  where
    method p' (EPrimitive _ v) = return (Slot p' v)
    method p' e' = gets top >>= \t -> return (Responder p' t e')


-- | Swap out a reference match with PThis, for inserting on an object.
setSelf :: Value -> Message Pattern -> Message Pattern
setSelf v (Keyword i ns ps os) =
    Keyword i ns (map (setSelf' v) ps) os
setSelf v (Single i n t os) =
    Single i n (setSelf' v t) os

setSelf' :: Value -> Pattern -> Pattern
setSelf' v (PMatch x) | v == x = PThis
setSelf' v (PMessage m) = PMessage $ setSelf v m
setSelf' v (PNamed n p') =
    PNamed n (setSelf' v p')
setSelf' v (PInstance p) = PInstance (setSelf' v p)
setSelf' v (PStrict p) = PStrict (setSelf' v p)
setSelf' _ p' = p'

-- | Pattern-match a value, inserting bindings into the current toplevel.
set :: Pattern -> Value -> VM Value
set p v = do
    is <- gets primitives
    if match is Nothing p v
        then do
            forM_ (bindings' p v) $ \(p', v') ->
                define p' (EPrimitive Nothing v')

            return v
        else throwError (Mismatch p v)


-- | Turn any PObject patterns into PMatches.
matchable :: Message Pattern -> VM (Message Pattern)
matchable p'@(Single { mTarget = t }) = do
    t' <- matchable' t
    return p' { mTarget = t' }
matchable p'@(Keyword { mTargets = ts }) = do
    ts' <- mapM matchable' ts
    return p' { mTargets = ts' }

matchable' :: Pattern -> VM Pattern
matchable' PThis = liftM PMatch (gets top)
matchable' (PObject oe) = liftM PMatch (eval oe)
matchable' (PInstance p) = liftM PInstance (matchable' p)
matchable' (PStrict p) = liftM PStrict (matchable' p)
matchable' (PVariable p) = liftM PVariable (matchable' p)
matchable' (PNamed n p') = liftM (PNamed n) (matchable' p')
matchable' (PMessage m) = liftM PMessage (matchable m)
matchable' p' = return p'

-- | Find the target objects for a message pattern.
targets :: IDs -> Message Pattern -> VM [Value]
targets is (Single { mTarget = p }) = targets' is p
targets is (Keyword { mTargets = ps }) = targets' is (head ps)

-- | Find the target objects for a pattern.
targets' :: IDs -> Pattern -> VM [Value]
targets' _ (PMatch v) = liftM (: []) (objectFor v)
targets' is (PNamed _ p) = targets' is p
targets' is PAny = return [idObject is]
targets' is (PList _) = return [idList is]
targets' is (PTuple _) = return [idTuple is]
targets' is (PHeadTail h t) = do
    ht <- targets' is h
    tt <- targets' is t
    if idCharacter is `elem` ht || idString is `elem` tt
        then return [idList is, idString is]
        else return [idList is]
targets' is (PPMKeyword {}) = return [idParticle is]
targets' is (PExpr _) = return [idExpression is]
targets' is (PInstance p) = targets' is p
targets' is (PStrict p) = targets' is p
targets' is (PVariable _) = return [idObject is]
targets' is (PMessage m) = targets is m
targets' _ p = error $ "no targets for " ++ show p



-----------------------------------------------------------------------------
-- Dispatch -----------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Dispatch a message to all roles and return a value.
--
-- If the message is not understood, @\@did-not-understand:(at:)@ is sent to all
-- roles until one responds to it. If none of them handle it, a
-- @\@did-not-understand:@ error is raised.
dispatch :: Message Value -> VM Value
dispatch !m = do
    find <- findMethod (target m) m
    case find of
        Just method -> runMethod method m
        Nothing ->
            case m of
                Single { mTarget = t } -> sendDNU t
                Keyword { mTargets = ts } -> sendDNUs ts 0
  where
    target (Single { mTarget = t }) = t
    target (Keyword { mTargets = ts }) = head ts

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


-- | Find a method on an object that responds to a given message, searching
-- its delegates if necessary.
findMethod :: Value -> Message Value -> VM (Maybe Method)
findMethod v m = do
    is <- gets primitives
    o <- objectFor v
    ms <- liftIO (readIORef (oMethods o))
    case relevant is o ms m of
        Nothing -> findFirstMethod m (oDelegates o)
        mt -> return mt

-- | Find the first value that has a method defiend for a given message.
findFirstMethod :: Message Value -> [Value] -> VM (Maybe Method)
findFirstMethod _ [] = return Nothing
findFirstMethod m (v:vs) = do
    r <- findMethod v m
    case r of
        Nothing -> findFirstMethod m vs
        _ -> return r

-- | Find a method on an object that responds to a given message.
relevant :: IDs -> Value -> (MethodMap, MethodMap) -> Message Value -> Maybe Method
relevant ids o ms m =
    lookupMap (mID m) (methods m) >>= firstMatch ids (Just o) m
  where
    methods (Single {}) = fst ms
    methods (Keyword {}) = snd ms

    firstMatch _ _ _ [] = Nothing
    firstMatch ids' r' m' (mt:mts)
        | match ids' r' (PMessage (mPattern mt)) (Message m') = Just mt
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
runMethod :: Method -> Message Value -> VM Value
runMethod (Slot { mValue = v }) _ = return v
runMethod (Responder { mPattern = p, mContext = c, mExpr = e }) m = do
    nt <- newObject [c] (bindings p m, emptyMap)

    forM_ (mOptionals p) $ \(Option i n (PObject oe)) ->
        case filter (\(Option x _ _) -> x == i) (mOptionals m) of
            [] -> do
                d <- withTop nt (eval oe)
                define (Single i n (PMatch nt) [])
                    (EPrimitive Nothing d)
            (Option oi on ov:_) ->
                define (Single oi on (PMatch nt) [])
                    (EPrimitive Nothing ov)

    withTop nt $ eval e
runMethod (Macro { mPattern = p, mExpr = e }) m = do
    t <- gets (psEnvironment . parserState)
    nt <- newObject [t] (bindings p m, emptyMap)

    forM_ (mOptionals p) $ \(Option i n (PExpr d)) ->
        case filter (\(Option x _ _) -> x == i) (mOptionals m) of
            [] ->
                define (Single i n (PMatch nt) [])
                    (EPrimitive Nothing (Expression d))
            (Option oi on ov:_) ->
                define (Single oi on (PMatch nt) [])
                    (EPrimitive Nothing ov)

    withTop nt $ eval e

-- | Get the object for a value.
objectFor :: Value -> VM Value
{-# INLINE objectFor #-}
objectFor !v = gets primitives >>= \is -> return $ objectFrom is v

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
{-throwError e = error ("panic: " ++ show (pretty e))-}
throwError e = gets top >>= \t -> do
    r <- dispatch (keyword ["responds-to?"] [t, particle "Error"])

    if r == Boolean True
        then do
            dispatch (msg t)
            error ("panic: error returned normally for: " ++ show e)
        else error ("panic: " ++ show (pretty e))
  where
    msg t = keyword ["error"] [t, asValue e]

