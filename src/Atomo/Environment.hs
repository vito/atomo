{-# LANGUAGE BangPatterns #-}
module Atomo.Environment where

import "monads-fd" Control.Monad.Cont
import "monads-fd" Control.Monad.State
import Data.Dynamic
import Data.IORef
import Data.List (nub)
import Data.Maybe (isJust)
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Vector as V

import Atomo.Method
import Atomo.Types


-- | evaluation
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

-- | evaluating multiple expressions, returning the last result
evalAll :: [Expr] -> VM Value
evalAll [] = throwError NoExpressions
evalAll [e] = eval e
evalAll (e:es) = eval e >> evalAll es

-- | object creation
newObject :: (Object -> Object) -> VM Value
newObject f = liftM Reference . liftIO $
    newIORef . f $ Object
        { oDelegates = []
        , oMethods = noMethods
        }

-- | run x with t as its toplevel object
withTop :: Value -> VM Value -> VM Value
withTop t x = do
    o <- gets top
    modify (\e -> e { top = t })

    res <- x

    modify (\e -> e { top = o })

    return res


-----------------------------------------------------------------------------
-- Define -------------------------------------------------------------------
-----------------------------------------------------------------------------

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

-- | define a pattern to evaluate an expression
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


-- | Swap out a reference match with PThis, for inserting on the object
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


set :: Pattern -> Value -> VM Value
set p v = do
    is <- gets primitives
    if match is Nothing p v
        then do
            forM_ (bindings' p v) $ \(p', v') ->
                define p' (Primitive Nothing v')

            return v
        else throwError (Mismatch p v)


-- | turn any PObject patterns into PMatches
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


-- | find the target objects for a pattern
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

-- | dispatch a message and return a value
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


-- | find a method on object `o' that responds to `m', searching its
-- delegates if necessary
findMethod :: Value -> Message -> VM (Maybe Method)
findMethod v m = do
    is <- gets primitives
    r <- orefFor v
    o <- liftIO (readIORef r)
    case relevant is r o m of
        Nothing -> findFirstMethod m (oDelegates o)
        mt -> return mt

-- | find the first value that has a method defiend for `m'
findFirstMethod :: Message -> [Value] -> VM (Maybe Method)
findFirstMethod _ [] = return Nothing
findFirstMethod m (v:vs) = do
    r <- findMethod v m
    case r of
        Nothing -> findFirstMethod m vs
        _ -> return r

-- | find a relevant method for message `m' on object `o'
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

-- | check if a value matches a given pattern
-- note that this is much faster when pure, so it uses unsafePerformIO
-- to check things like delegation matches.
match :: IDs -> Maybe ORef -> Pattern -> Value -> Bool
{-# NOINLINE match #-}
match ids (Just r) PThis (Reference y) =
    refMatch ids (Just r) r y
match ids (Just r) PThis y =
    match ids (Just r) (PMatch (Reference r)) (Reference (orefFrom ids y))
match _ _ (PMatch x) y | x == y = True
match ids r (PMatch x) (Reference y) =
    delegatesMatch ids r (PMatch x) y
match ids r (PMatch (Reference x)) y =
    match ids r (PMatch (Reference x)) (Reference (orefFrom ids y))
match ids r
    (PSingle { ppTarget = p })
    (Message (Single { mTarget = t })) =
    match ids r p t
match ids r
    (PKeyword { ppTargets = ps })
    (Message (Keyword { mTargets = ts })) =
    matchAll ids r ps ts
match ids r (PInstance p) (Reference o) = delegatesMatch ids r p o
match ids r (PInstance p) v = match ids r p v
match _ _ (PStrict (PMatch x)) v = x == v
match ids r (PStrict p) v = match ids r p v
match ids r (PNamed _ p) v = match ids r p v
match _ _ PAny _ = True
match ids r (PList ps) (List v) = matchAll ids r ps (V.toList v)
match ids r (PHeadTail hp tp) (List vs) =
    V.length vs > 0 && match ids r hp h && match ids r tp t
  where
    h = V.head vs
    t = List (V.tail vs)
match ids r (PHeadTail hp tp) (String t) | not (T.null t) =
    match ids r hp (Char (T.head t)) && match ids r tp (String (T.tail t))
match ids r (PPMKeyword ans aps) (Particle (PMKeyword bns mvs)) =
    ans == bns && matchParticle ids r aps mvs
match _ _ PEDefine (Expression (Define {})) = True
match _ _ PESet (Expression (Set {})) = True
match _ _ PEDispatch (Expression (Dispatch {})) = True
match _ _ PEOperator (Expression (Operator {})) = True
match _ _ PEPrimitive (Expression (Primitive {})) = True
match _ _ PEBlock (Expression (EBlock {})) = True
match _ _ PEList (Expression (EList {})) = True
match _ _ PEMacro (Expression (EMacro {})) = True
match _ _ PEParticle (Expression (EParticle {})) = True
match _ _ PETop (Expression (ETop {})) = True
match _ _ PEQuote (Expression (EQuote {})) = True
match _ _ PEUnquote (Expression (EUnquote {})) = True
match _ _ (PExpr a) (Expression b) = matchExpr 0 a b
match ids r p (Reference y) = delegatesMatch ids r p y
match _ _ _ _ = False

refMatch :: IDs -> Maybe ORef -> ORef -> ORef -> Bool
refMatch ids r x y = x == y || delegatesMatch ids r (PMatch (Reference x)) y

delegatesMatch :: IDs -> Maybe ORef -> Pattern -> ORef -> Bool
delegatesMatch ids mr p x =
    any (match ids mr p) (oDelegates (unsafePerformIO (readIORef x)))

-- | match multiple patterns with multiple values
matchAll :: IDs -> Maybe ORef -> [Pattern] -> [Value] -> Bool
matchAll _ _ [] [] = True
matchAll ids mr (p:ps) (v:vs) = match ids mr p v && matchAll ids mr ps vs
matchAll _ _ _ _ = False

matchEParticle :: Int -> [Maybe Expr] -> [Maybe Expr] -> Bool
matchEParticle _ [] [] = True
matchEParticle n (Just a:as) (Just b:bs) =
    matchExpr n a b && matchEParticle n as bs
matchEParticle n (Nothing:as) (Nothing:bs) = matchEParticle n as bs
matchEParticle _ _ _ = False

matchExpr :: Int -> Expr -> Expr -> Bool
matchExpr 0 (EUnquote {}) _ = True
matchExpr n (EUnquote { eExpr = a }) (EUnquote { eExpr = b }) =
    matchExpr (n - 1) a b
matchExpr n (Define { ePattern = ap', eExpr = a }) (Define { ePattern = bp, eExpr = b }) =
    ap' == bp && matchExpr n a b
matchExpr n (Set { ePattern = ap', eExpr = a }) (Set { ePattern = bp, eExpr = b }) =
    ap' == bp && matchExpr n a b
matchExpr n (Dispatch { eMessage = am@(EKeyword {}) }) (Dispatch { eMessage = bm@(EKeyword {}) }) =
    emID am == emID bm && length (emTargets am) == length (emTargets bm) && and (zipWith (matchExpr n) (emTargets am) (emTargets bm))
matchExpr n (Dispatch { eMessage = am@(ESingle {}) }) (Dispatch { eMessage = bm@(ESingle {}) }) =
    emID am == emID bm && matchExpr n (emTarget am) (emTarget bm)
matchExpr n (EBlock { eArguments = aps, eContents = as }) (EBlock { eArguments = bps, eContents = bs }) =
    aps == bps && length as == length bs && and (zipWith (matchExpr n) as bs)
matchExpr n (EList { eContents = as }) (EList { eContents = bs }) =
    length as == length bs && and (zipWith (matchExpr n) as bs)
matchExpr n (EMacro { ePattern = ap', eExpr = a }) (EMacro { ePattern = bp, eExpr = b }) =
    ap' == bp && matchExpr n a b
matchExpr n (EParticle { eParticle = ap' }) (EParticle { eParticle = bp }) =
    case (ap', bp) of
        (EPMKeyword ans ames, EPMKeyword bns bmes) ->
            ans == bns && matchEParticle n ames bmes
        _ -> ap' == bp
matchExpr n (EQuote { eExpr = a }) (EQuote { eExpr = b }) =
    matchExpr (n + 1) a b
matchExpr _ a b = a == b

matchParticle :: IDs -> Maybe ORef -> [Pattern] -> [Maybe Value] -> Bool
matchParticle _ _ [] [] = True
matchParticle ids mr (PAny:ps) (Nothing:mvs) = matchParticle ids mr ps mvs
matchParticle ids mr (PNamed _ p:ps) mvs = matchParticle ids mr (p:ps) mvs
matchParticle ids mr (p:ps) (Just v:mvs) =
    match ids mr p v && matchParticle ids mr ps mvs
matchParticle _ _ _ _ = False

-- | evaluate a method in a scope with the pattern's bindings, delegating to
-- the method's context and setting the "dispatch" object
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

-- | evaluate an action in a new scope
newScope :: VM Value -> VM Value
newScope x = do
    t <- gets top
    nt <- newObject $ \o -> o
        { oDelegates = [t]
        }

    withTop nt x

-- | given a pattern and a message, return the bindings from the pattern
bindings :: Pattern -> Message -> MethodMap
bindings (PSingle { ppTarget = p }) (Single { mTarget = t }) =
    toMethods (bindings' p t)
bindings (PKeyword { ppTargets = ps }) (Keyword { mTargets = ts }) =
    toMethods $ concat (zipWith bindings' ps ts)
bindings p m = error $ "impossible: bindings on " ++ show (p, m)

-- | given a pattern and avalue, return the bindings as a list of pairs
bindings' :: Pattern -> Value -> [(Pattern, Value)]
bindings' (PNamed n p) v = (psingle n PThis, v) : bindings' p v
bindings' (PPMKeyword _ ps) (Particle (PMKeyword _ mvs)) =
    concatMap (\(p, Just v) -> bindings' p v)
    $ filter (isJust . snd)
    $ zip ps mvs
bindings' (PList ps) (List vs) = concat (zipWith bindings' ps (V.toList vs))
bindings' (PHeadTail hp tp) (List vs) =
    bindings' hp h ++ bindings' tp t
  where
    h = V.head vs
    t = List (V.tail vs)
bindings' (PHeadTail hp tp) (String t) | not (T.null t) =
    bindings' hp (Char (T.head t)) ++ bindings' tp (String (T.tail t))
bindings' (PExpr a) (Expression b) = exprBindings 0 a b
bindings' (PInstance p) (Reference r) =
    concatMap (bindings' p) $ oDelegates (unsafePerformIO (readIORef r))
bindings' (PInstance p) v = bindings' p v
bindings' (PStrict p) v = bindings' p v
bindings' p (Reference r) =
    concatMap (bindings' p) $ oDelegates (unsafePerformIO (readIORef r))
bindings' _ _ = []


exprBindings :: Int -> Expr -> Expr -> [(Pattern, Value)]
exprBindings 0 (EUnquote { eExpr = Dispatch { eMessage = ESingle { emName = n } } }) e =
    [(psingle n PThis, Expression e)]
exprBindings n (EUnquote { eExpr = a }) (EUnquote { eExpr = b }) =
    exprBindings (n - 1) a b
exprBindings n (Define { eExpr = a }) (Define { eExpr = b }) =
    exprBindings n a b
exprBindings n (Set { eExpr = a }) (Set { eExpr = b }) =
    exprBindings n a b
exprBindings n (Dispatch { eMessage = am@(EKeyword {}) }) (Dispatch { eMessage = bm@(EKeyword {}) }) =
    concat $ zipWith (exprBindings n) (emTargets am) (emTargets bm)
exprBindings n (Dispatch { eMessage = am@(ESingle {}) }) (Dispatch { eMessage = bm@(ESingle {}) }) =
    exprBindings n (emTarget am) (emTarget bm)
exprBindings n (EBlock { eContents = as }) (EBlock { eContents = bs }) =
    concat $ zipWith (exprBindings n) as bs
exprBindings n (EList { eContents = as }) (EList { eContents = bs }) =
    concat $ zipWith (exprBindings n) as bs
exprBindings n (EMacro { eExpr = a }) (EMacro { eExpr = b }) =
    exprBindings n a b
exprBindings n (EParticle { eParticle = ap' }) (EParticle { eParticle = bp }) =
    case (ap', bp) of
        (EPMKeyword _ ames, EPMKeyword _ bmes) ->
            concatMap (\(Just a, Just b) -> exprBindings n a b)
            $ filter (isJust . fst)
            $ zip ames bmes
        _ -> []
exprBindings n (EQuote { eExpr = a }) (EQuote { eExpr = b }) =
    exprBindings (n + 1) a b
exprBindings _ _ _ = []


-----------------------------------------------------------------------------
-- Helpers ------------------------------------------------------------------
-----------------------------------------------------------------------------

infixr 0 =:, =::

-- | define a method as an action returning a value
(=:) :: Pattern -> VM Value -> VM ()
pat =: vm = define pat (EVM Nothing Nothing vm)

-- | define a slot to a given value
(=::) :: Pattern -> Value -> VM ()
pat =:: v = define pat (Primitive Nothing v)

-- | define a method that evaluates e
(=:::) :: Pattern -> Expr -> VM ()
pat =::: e = define pat e

-- | find a value from an object, searching its delegates, throwing
-- a descriptive error if it is not found
findValue :: String -> (Value -> Bool) -> Value -> VM Value
findValue _ t v | t v = return v
findValue d t v = findValue' t v >>= maybe die return
  where
    die = throwError (ValueNotFound d v)

-- | findValue, but returning Nothing instead of failing
findValue' :: (Value -> Bool) -> Value -> VM (Maybe Value)
findValue' t v | t v = return (Just v)
findValue' t (Reference r) = do
    o <- liftIO (readIORef r)
    findDels (oDelegates o)
  where
    findDels [] = return Nothing
    findDels (d:ds) = do
        f <- findValue' t d
        case f of
            Nothing -> findDels ds
            Just v -> return (Just v)
findValue' _ _ = return Nothing

findBlock :: Value -> VM Value
findBlock v
    | isBlock v = return v
    | otherwise = findValue "Block" isBlock v

findBoolean :: Value -> VM Value
findBoolean v
    | isBoolean v = return v
    | otherwise = findValue "Boolean" isBoolean v

findChar :: Value -> VM Value
findChar v
    | isChar v = return v
    | otherwise = findValue "Char" isChar v

findContinuation :: Value -> VM Value
findContinuation v
    | isContinuation v = return v
    | otherwise = findValue "Continuation" isContinuation v

findDouble :: Value -> VM Value
findDouble v
    | isDouble v = return v
    | otherwise = findValue "Double" isDouble v

findExpression :: Value -> VM Value
findExpression v
    | isExpression v = return v
    | otherwise = findValue "Expression" isExpression v

findHaskell :: Value -> VM Value
findHaskell v
    | isHaskell v = return v
    | otherwise = findValue "Haskell" isHaskell v

findInteger :: Value -> VM Value
findInteger v
    | isInteger v = return v
    | otherwise = findValue "Integer" isInteger v

findList :: Value -> VM Value
findList v
    | isList v = return v
    | otherwise = findValue "List" isList v

findMessage :: Value -> VM Value
findMessage v
    | isMessage v = return v
    | otherwise = findValue "Message" isMessage v

findMethod' :: Value -> VM Value
findMethod' v
    | isMethod v = return v
    | otherwise = findValue "Method" isMethod v

findParticle :: Value -> VM Value
findParticle v
    | isParticle v = return v
    | otherwise = findValue "Particle" isParticle v

findProcess :: Value -> VM Value
findProcess v
    | isProcess v = return v
    | otherwise = findValue "Process" isProcess v

findPattern :: Value -> VM Value
findPattern v
    | isPattern v = return v
    | otherwise = findValue "Pattern" isPattern v

findRational :: Value -> VM Value
findRational v
    | isRational v = return v
    | otherwise = findValue "Rational" isRational v

findReference :: Value -> VM Value
findReference v
    | isReference v = return v
    | otherwise = findValue "Reference" isReference v

findString :: Value -> VM Value
findString v
    | isString v = return v
    | otherwise = findValue "String" isString v

getString :: Expr -> VM String
getString e = eval e >>= liftM (fromText . fromString) . findString

getText :: Expr -> VM T.Text
getText e = eval e >>= findString >>= \(String t) -> return t

getList :: Expr -> VM [Value]
getList = liftM V.toList . getVector

getVector :: Expr -> VM (V.Vector Value)
getVector e = eval e
    >>= findList
    >>= \(List v) -> return v

here :: String -> VM Value
here n = gets top >>= dispatch . single n

ifVM :: VM Value -> VM a -> VM a -> VM a
ifVM c a b = do
    r <- c
    if r == Boolean True then a else b

ifVM' :: VM Bool -> VM a -> VM a -> VM a
ifVM' c a b = do
    r <- c
    if r then a else b

ifE :: Expr -> VM a -> VM a -> VM a
ifE = ifVM . eval

referenceTo :: Value -> VM Value
{-# INLINE referenceTo #-}
referenceTo = liftM Reference . orefFor

callBlock :: Value -> [Value] -> VM Value
callBlock (Block s ps es) vs = do
    is <- gets primitives
    checkArgs is ps vs
    doBlock (toMethods . concat $ zipWith bindings' ps vs) s es
  where
    checkArgs _ [] _ = return (particle "ok")
    checkArgs _ _ [] = throwError (BlockArity (length ps) (length vs))
    checkArgs is (p:ps') (v:vs')
        | match is Nothing p v = checkArgs is ps' vs'
        | otherwise = throwError (Mismatch p v)
callBlock x _ = raise ["not-a-block"] [x]

doBlock :: MethodMap -> Value -> [Expr] -> VM Value
{-# INLINE doBlock #-}
doBlock bms s es = do
    blockScope <- newObject $ \o -> o
        { oDelegates = [s]
        , oMethods = (bms, emptyMap)
        }

    withTop blockScope (evalAll es)

objectFor :: Value -> VM Object
{-# INLINE objectFor #-}
objectFor v = orefFor v >>= liftIO . readIORef

orefFor :: Value -> VM ORef
{-# INLINE orefFor #-}
orefFor !v = gets primitives >>= \is -> return $ orefFrom is v

orefFrom :: IDs -> Value -> ORef
{-# INLINE orefFrom #-}
orefFrom _ (Reference r) = r
orefFrom ids (Block _ _ _) = idBlock ids
orefFrom ids (Boolean _) = idBoolean ids
orefFrom ids (Char _) = idChar ids
orefFrom ids (Continuation _) = idContinuation ids
orefFrom ids (Double _) = idDouble ids
orefFrom ids (Expression _) = idExpression ids
orefFrom ids (Haskell _) = idHaskell ids
orefFrom ids (Integer _) = idInteger ids
orefFrom ids (List _) = idList ids
orefFrom ids (Message _) = idMessage ids
orefFrom ids (Method _) = idMethod ids
orefFrom ids (Particle _) = idParticle ids
orefFrom ids (Process _ _) = idProcess ids
orefFrom ids (Pattern _) = idPattern ids
orefFrom ids (Rational _) = idRational ids
orefFrom ids (String _) = idString ids

-- | does one value delegate to another?
delegatesTo :: Value -> Value -> VM Bool
delegatesTo f t = do
    o <- objectFor f
    delegatesTo' (oDelegates o)
  where
    delegatesTo' [] = return False
    delegatesTo' (d:ds)
        | t `elem` (d:ds) = return True
        | otherwise = do
            o <- objectFor d
            delegatesTo' (oDelegates o ++ ds)

-- | is one value an instance of, equal to, or a delegation to another?
-- for example, 1 is-a?: Integer, but 1 does not delegates-to?: Integer
isA :: Value -> Value -> VM Bool
isA x y = do
    xr <- orefFor x
    yr <- orefFor y

    if xr == yr
        then return True
        else do
            ds <- liftM oDelegates (objectFor x)
            isA' ds
  where
    isA' [] = return False
    isA' (d:ds) = do
        di <- isA d y
        if di
            then return True
            else isA' ds

raise :: [String] -> [Value] -> VM a
{-# INLINE raise #-}
raise ns vs = throwError . Error $ keyParticleN ns vs

raise' :: String -> VM a
{-# INLINE raise' #-}
raise' = throwError . Error . particle

fromHaskell :: Typeable a => String -> Value -> VM a
fromHaskell t (Haskell d) =
    case fromDynamic d of
        Just a -> return a
        Nothing -> raise ["dynamic-needed"] [string t]
fromHaskell t _ = raise ["dynamic-needed"] [string t]

throwError :: AtomoError -> VM a
throwError e = gets top >>= \t ->
    ifVM (dispatch (keyword ["responds-to?"] [t, particle "Error"]))
        (dispatch (msg t) >> error ("panic: error returned normally for: " ++ show e))
        (error ("panic: " ++ show e))
  where
    msg t = keyword ["error"] [t, asValue e]

toPattern' :: Expr -> VM Pattern
toPattern' = tryPattern toPattern

toDefinePattern' :: Expr -> VM Pattern
toDefinePattern' = tryPattern toDefinePattern

toRolePattern' :: Expr -> VM Pattern
toRolePattern' = tryPattern toRolePattern

toMacroPattern' :: Expr -> VM Pattern
toMacroPattern' = tryPattern toMacroPattern

tryPattern :: (Expr -> Maybe Pattern) -> Expr -> VM Pattern
tryPattern c e = 
    case c e of
        Nothing -> raise ["unknown-pattern"] [Expression e]
        Just p -> return p
