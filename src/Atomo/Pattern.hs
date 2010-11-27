module Atomo.Pattern
    ( bindings
    , bindings'
    , match
    , matchAll
    , toPattern
    , toDefinePattern
    , toRolePattern
    , toMacroPattern
    , toMacroRole
    ) where

import Control.Monad (forM, liftM)
import Data.Char (isUpper)
import Data.IORef (readIORef)
import Data.Maybe (isJust)
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Vector as V

import Atomo.Method
import Atomo.Types


-- | Check if a value matches a given pattern.
--
-- Note that this is much faster when pure, so it uses unsafePerformIO
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
    (PMessage (Single { mTarget = p }))
    ( Message (Single { mTarget = t })) =
    match ids r p t
match ids r
    (PMessage (Keyword { mTargets = ps }))
    ( Message (Keyword { mTargets = ts })) =
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

-- | Check if two references are equal or if one delegates to another.
refMatch :: IDs -> Maybe ORef -> ORef -> ORef -> Bool
refMatch ids r x y = x == y || delegatesMatch ids r (PMatch (Reference x)) y

-- | Check if an object's delegates match a pattern.
delegatesMatch :: IDs -> Maybe ORef -> Pattern -> ORef -> Bool
delegatesMatch ids mr p x =
    any (match ids mr p) (oDelegates (unsafePerformIO (readIORef x)))

-- | Match multiple patterns with multiple values.
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
matchExpr n (Define { emPattern = ap', eExpr = a }) (Define { emPattern = bp, eExpr = b }) =
    ap' == bp && matchExpr n a b
matchExpr n (Set { ePattern = ap', eExpr = a }) (Set { ePattern = bp, eExpr = b }) =
    ap' == bp && matchExpr n a b
matchExpr n (Dispatch { eMessage = am@(Keyword {}) }) (Dispatch { eMessage = bm@(Keyword {}) }) =
    mID am == mID bm && length (mTargets am) == length (mTargets bm) && and (zipWith (matchExpr n) (mTargets am) (mTargets bm))
matchExpr n (Dispatch { eMessage = am@(Single {}) }) (Dispatch { eMessage = bm@(Single {}) }) =
    mID am == mID bm && matchExpr n (mTarget am) (mTarget bm)
matchExpr n (EBlock { eArguments = aps, eContents = as }) (EBlock { eArguments = bps, eContents = bs }) =
    aps == bps && length as == length bs && and (zipWith (matchExpr n) as bs)
matchExpr n (EList { eContents = as }) (EList { eContents = bs }) =
    length as == length bs && and (zipWith (matchExpr n) as bs)
matchExpr n (EMacro { emPattern = ap', eExpr = a }) (EMacro { emPattern = bp, eExpr = b }) =
    ap' == bp && matchExpr n a b
matchExpr n (EParticle { eParticle = ap' }) (EParticle { eParticle = bp }) =
    case (ap', bp) of
        (PMKeyword ans ames, PMKeyword bns bmes) ->
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

-- | Given a pattern and a message, return the bindings from the pattern.
bindings :: Message Pattern -> Message Value -> MethodMap
bindings (Single { mTarget = p }) (Single { mTarget = t }) =
    toMethods (bindings' p t)
bindings (Keyword { mTargets = ps }) (Keyword { mTargets = ts }) =
    toMethods $ concat (zipWith bindings' ps ts)
bindings p m = error $ "impossible: bindings on " ++ show (p, m)

-- | Given a pattern and avalue, return the bindings as a list of pairs.
bindings' :: Pattern -> Value -> [(Message Pattern, Value)]
bindings' (PNamed n p) v = (single n PThis, v) : bindings' p v
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


exprBindings :: Int -> Expr -> Expr -> [(Message Pattern, Value)]
exprBindings 0 (EUnquote { eExpr = Dispatch { eMessage = Single { mName = n } } }) e =
    [(single n PThis, Expression e)]
exprBindings n (EUnquote { eExpr = a }) (EUnquote { eExpr = b }) =
    exprBindings (n - 1) a b
exprBindings n (Define { eExpr = a }) (Define { eExpr = b }) =
    exprBindings n a b
exprBindings n (Set { eExpr = a }) (Set { eExpr = b }) =
    exprBindings n a b
exprBindings n (Dispatch { eMessage = am@(Keyword {}) }) (Dispatch { eMessage = bm@(Keyword {}) }) =
    concat $ zipWith (exprBindings n) (mTargets am) (mTargets bm)
exprBindings n (Dispatch { eMessage = am@(Single {}) }) (Dispatch { eMessage = bm@(Single {}) }) =
    exprBindings n (mTarget am) (mTarget bm)
exprBindings n (EBlock { eContents = as }) (EBlock { eContents = bs }) =
    concat $ zipWith (exprBindings n) as bs
exprBindings n (EList { eContents = as }) (EList { eContents = bs }) =
    concat $ zipWith (exprBindings n) as bs
exprBindings n (EMacro { eExpr = a }) (EMacro { eExpr = b }) =
    exprBindings n a b
exprBindings n (EParticle { eParticle = ap' }) (EParticle { eParticle = bp }) =
    case (ap', bp) of
        (PMKeyword _ ames, PMKeyword _ bmes) ->
            concatMap (\(Just a, Just b) -> exprBindings n a b)
            $ filter (isJust . fst)
            $ zip ames bmes
        _ -> []
exprBindings n (EQuote { eExpr = a }) (EQuote { eExpr = b }) =
    exprBindings (n + 1) a b
exprBindings _ _ _ = []

-- | Convert an expression to the pattern match it represents.
toPattern :: Expr -> Maybe Pattern
toPattern (Dispatch { eMessage = Keyword { mNames = ["."], mTargets = [h, t] } }) = do
    hp <- toPattern h
    tp <- toPattern t
    return (PHeadTail hp tp)
toPattern (Dispatch { eMessage = Keyword { mNames = ["->"], mTargets = [ETop {}, o] } }) = do
    liftM PInstance (toPattern o)
toPattern (Dispatch { eMessage = Keyword { mNames = ["=="], mTargets = [ETop {}, o] } }) = do
    liftM PStrict (toPattern o)
toPattern (Dispatch { eMessage = Keyword { mNames = [n], mTargets = [ETop {}, x] } }) = do
    p <- toPattern x
    return (PNamed n p)
toPattern (Dispatch { eMessage = Keyword { mNames = ns, mTargets = ts } }) =
    return (pkeyword ns (map PObject ts))
toPattern (Dispatch { eMessage = Single { mName = "_" } }) =
    return PAny
toPattern (Dispatch { eMessage = Single { mName = n, mTarget = ETop {} } }) =
    return (PNamed n PAny)
toPattern (Dispatch { eMessage = Single { mTarget = d@(Dispatch {}), mName = n } }) =
    return (psingle n (PObject d))
toPattern (EList { eContents = es }) = do
    ps <- mapM toPattern es
    return (PList ps)
toPattern (EParticle { eParticle = PMSingle n }) =
    return (PMatch (Particle (PMSingle n)))
toPattern (EParticle { eParticle = PMKeyword ns mes }) = do
    ps <- forM mes $ \me ->
        case me of
            Nothing -> return PAny
            Just e -> toPattern e

    return (PPMKeyword ns ps)
toPattern (EQuote { eExpr = e }) = return (PExpr e)
toPattern (Primitive { eValue = v }) =
    return (PMatch v)
toPattern (ETop {}) =
    return (PObject (ETop Nothing))
toPattern b@(EBlock {}) =
    return (PObject (Dispatch Nothing (keyword ["call-in"] [b, ETop Nothing])))
toPattern _ = Nothing

-- | Convert an expression into a definition's message pattern.
toDefinePattern :: Expr -> Maybe (Message Pattern)
toDefinePattern (Dispatch { eMessage = Single { mName = n, mTarget = t } }) = do
    p <- toRolePattern t
    return (single n p)
toDefinePattern (Dispatch { eMessage = Keyword { mNames = ns, mTargets = ts } }) = do
    ps <- mapM toRolePattern ts
    return (keyword ns ps)
toDefinePattern _ = Nothing

-- | Convert an expression into a pattern-match for use as a message's role.
toRolePattern :: Expr -> Maybe Pattern
toRolePattern (Dispatch { eMessage = Keyword { mNames = ["->"], mTargets = [ETop {}, o] } }) = do
    liftM PInstance (toRolePattern o)
toRolePattern (Dispatch { eMessage = Keyword { mNames = ["=="], mTargets = [ETop {}, o] } }) = do
    liftM PStrict (toRolePattern o)
toRolePattern (Dispatch { eMessage = Keyword { mNames = [n], mTargets = [ETop {}, x] } }) = do
    p <- toRolePattern x
    return (PNamed n p)
toRolePattern d@(Dispatch { eMessage = Single { mTarget = ETop {}, mName = n } })
    | isUpper (head n) = return (PObject d)
    | otherwise = return (PNamed n PAny)
toRolePattern d@(Dispatch { eMessage = Single { mTarget = (Dispatch {}) } }) =
    return (PObject d)
toRolePattern p = toPattern p

-- | Convert an expression into a macro's message pattern.
toMacroPattern :: Expr -> Maybe (Message Pattern)
toMacroPattern (Dispatch { eMessage = Single { mName = n, mTarget = t } }) = do
    p <- toMacroRole t
    return (single n p)
toMacroPattern (Dispatch { eMessage = Keyword { mNames = ns, mTargets = ts } }) = do
    ps <- mapM toMacroRole ts
    return (keyword ns ps)
toMacroPattern _ = Nothing

-- | Convert an expression into a pattern-match for use as a macro's role.
toMacroRole :: Expr -> Maybe Pattern
toMacroRole (Dispatch _ (Single _ "Dispatch" _)) = Just PEDispatch
toMacroRole (Dispatch _ (Single _ "Operator" _)) = Just PEOperator
toMacroRole (Dispatch _ (Single _ "Primitive" _)) = Just PEPrimitive
toMacroRole (Dispatch _ (Single _ "Block" _)) = Just PEBlock
toMacroRole (Dispatch _ (Single _ "List" _)) = Just PEList
toMacroRole (Dispatch _ (Single _ "Macro" _)) = Just PEMacro
toMacroRole (Dispatch _ (Single _ "Particle" _)) = Just PEParticle
toMacroRole (Dispatch _ (Single _ "Top" _)) = Just PETop
toMacroRole (Dispatch _ (Single _ "Quote" _)) = Just PEQuote
toMacroRole (Dispatch _ (Single _ "Unquote" _)) = Just PEUnquote
toMacroRole (Dispatch { eMessage = Keyword { mNames = [n], mTargets = [ETop {}, x] } }) = do
    p <- toMacroRole x
    return (PNamed n p)
toMacroRole (ETop {}) = Just PAny
toMacroRole p = toPattern p
