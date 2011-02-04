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
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import qualified Data.Vector as V

import Atomo.Method
import Atomo.Types


-- | Check if a value matches a given pattern.
match :: IDs -> Maybe Value -> Pattern -> Value -> Bool
{-# NOINLINE match #-}
match ids (Just r) PThis y@(Object { oDelegates = ds }) =
    r == y || any (match ids (Just r) (PMatch r)) ds
match ids (Just r) PThis y =
    match ids (Just r) (PMatch r) (objectFrom ids y)
match _ _ (PMatch x) y | x == y = True
match ids r x@(PMatch _) (Object { oDelegates = ds }) =
    any (match ids r x) ds
match ids r x@(PMatch (Object {})) y =
    match ids r x (objectFrom ids y)
match ids r
    (PMessage (Single { mTarget = p }))
    ( Message (Single { mTarget = t })) =
    match ids r p t
match ids r
    (PMessage (Keyword { mTargets = ps }))
    ( Message (Keyword { mTargets = ts })) =
    matchAll ids r ps ts
match ids r (PInstance p) (Object { oDelegates = ds }) =
    any (match ids r p) ds
match ids r (PInstance p) v = match ids r p v
match _ _ (PStrict (PMatch x)) v = x == v
match ids r (PStrict p) v = match ids r p v
match ids r (PVariable p) (Tuple t) = match ids r p (List t)
match ids r (PVariable p) v = match ids r p (list [v])
match ids r (PNamed _ p) v = match ids r p v
match _ _ PAny _ = True
match ids r (PList ps) (List v) = matchAll ids r ps (V.toList v)
match ids r (PHeadTail hp tp) (List vs) =
    V.length vs > 0 && match ids r hp h && match ids r tp t
  where
    h = V.head vs
    t = List (V.tail vs)
match ids r (PHeadTail hp tp) (String t) | not (T.null t) =
    match ids r hp (Character (T.head t)) && match ids r tp (String (T.tail t))
match ids r (PTuple ps) (Tuple v) = matchAll ids r ps (V.toList v)
match ids r (PPMKeyword ans aps) (Particle (Keyword { mNames = bns, mTargets = mvs })) =
    ans == bns && matchParticle ids r aps mvs
match ids r p (Object { oDelegates = ds }) =
    any (match ids r p) ds
match _ _ p (Expression e) = macroMatch p e
match _ _ _ _ = False

macroMatch :: Pattern -> Expr -> Bool
macroMatch PEDispatch (EDispatch {}) = True
macroMatch PEOperator (EOperator {}) = True
macroMatch PEPrimitive (EPrimitive {}) = True
macroMatch PEBlock (EBlock {}) = True
macroMatch PEList (EList {}) = True
macroMatch PETuple (ETuple {}) = True
macroMatch PEMacro (EMacro {}) = True
macroMatch PEParticle (EParticle {}) = True
macroMatch PETop (ETop {}) = True
macroMatch PEQuote (EQuote {}) = True
macroMatch PEUnquote (EUnquote {}) = True
macroMatch (PExpr a) b = matchExpr 0 a b
macroMatch _ _ = False

-- | Match multiple patterns with multiple values.
matchAll :: IDs -> Maybe Value -> [Pattern] -> [Value] -> Bool
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
matchExpr 0 (EUnquote { eExpr = EDispatch { eMessage = Single {} } }) _ = True
matchExpr 0 (EUnquote { eExpr = EDispatch { eMessage = Keyword { mTargets = [ETop {}, pat] } } }) e
    | isJust mp = macroMatch (fromJust mp) e --matchExpr 0 pat e
  where
    mp = toMacroRole pat
matchExpr n (EUnquote { eExpr = a }) (EUnquote { eExpr = b }) =
    matchExpr (n - 1) a b
matchExpr n (EDefine { emPattern = ap', eExpr = a }) (EDefine { emPattern = bp, eExpr = b }) =
    ap' == bp && matchExpr n a b
matchExpr n (ESet { ePattern = ap', eExpr = a }) (ESet { ePattern = bp, eExpr = b }) =
    ap' == bp && matchExpr n a b
matchExpr n (EDispatch { eMessage = am@(Keyword {}) }) (EDispatch { eMessage = bm@(Keyword {}) }) =
    mID am == mID bm && length (mTargets am) == length (mTargets bm) && and (zipWith (matchExpr n) (mTargets am) (mTargets bm))
matchExpr n (EDispatch { eMessage = am@(Single {}) }) (EDispatch { eMessage = bm@(Single {}) }) =
    mID am == mID bm && matchExpr n (mTarget am) (mTarget bm)
matchExpr n (EBlock { eArguments = aps, eContents = as }) (EBlock { eArguments = bps, eContents = bs }) =
    aps == bps && length as == length bs && and (zipWith (matchExpr n) as bs)
matchExpr n (EList { eContents = as }) (EList { eContents = bs }) =
    length as == length bs && and (zipWith (matchExpr n) as bs)
matchExpr n (ETuple { eContents = as }) (ETuple { eContents = bs }) =
    length as == length bs && and (zipWith (matchExpr n) as bs)
matchExpr n (EMacro { emPattern = ap', eExpr = a }) (EMacro { emPattern = bp, eExpr = b }) =
    ap' == bp && matchExpr n a b
matchExpr n (EParticle { eParticle = ap' }) (EParticle { eParticle = bp }) =
    case (ap', bp) of
        (Keyword { mNames = ans, mTargets = ames }, Keyword { mNames = bns, mTargets = bmes }) ->
            ans == bns && matchEParticle n ames bmes
        _ -> ap' == bp
matchExpr n (EQuote { eExpr = a }) (EQuote { eExpr = b }) =
    matchExpr (n + 1) a b
matchExpr _ a b = a == b

matchParticle :: IDs -> Maybe Value -> [Pattern] -> [Maybe Value] -> Bool
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
bindings' (PPMKeyword _ ps) (Particle (Keyword { mTargets = mvs })) =
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
    bindings' hp (Character (T.head t)) ++ bindings' tp (String (T.tail t))
bindings' (PTuple ps) (Tuple vs) = concat (zipWith bindings' ps (V.toList vs))
bindings' (PExpr a) (Expression b) = exprBindings 0 a b
bindings' (PInstance p) (Object { oDelegates = ds }) =
    concatMap (bindings' p) ds
bindings' (PInstance p) v = bindings' p v
bindings' (PStrict p) v = bindings' p v
bindings' (PVariable p) (Tuple t) = bindings' p (List t)
bindings' (PVariable p) v = bindings' p (list [v])
bindings' p (Object { oDelegates = ds }) =
    concatMap (bindings' p) ds
bindings' _ _ = []


exprBindings :: Int -> Expr -> Expr -> [(Message Pattern, Value)]
exprBindings 0 (EUnquote { eExpr = EDispatch { eMessage = Single { mName = n } } }) e =
    [(single n PThis, Expression e)]
exprBindings 0 (EUnquote { eExpr = EDispatch { eMessage = Keyword { mNames = [n] } } }) e =
    [(single n PThis, Expression e)]
exprBindings n (EUnquote { eExpr = a }) (EUnquote { eExpr = b }) =
    exprBindings (n - 1) a b
exprBindings n (EDefine { eExpr = a }) (EDefine { eExpr = b }) =
    exprBindings n a b
exprBindings n (ESet { eExpr = a }) (ESet { eExpr = b }) =
    exprBindings n a b
exprBindings n (EDispatch { eMessage = am@(Keyword {}) }) (EDispatch { eMessage = bm@(Keyword {}) }) =
    concat $ zipWith (exprBindings n) (mTargets am) (mTargets bm)
exprBindings n (EDispatch { eMessage = am@(Single {}) }) (EDispatch { eMessage = bm@(Single {}) }) =
    exprBindings n (mTarget am) (mTarget bm)
exprBindings n (EBlock { eContents = as }) (EBlock { eContents = bs }) =
    concat $ zipWith (exprBindings n) as bs
exprBindings n (EList { eContents = as }) (EList { eContents = bs }) =
    concat $ zipWith (exprBindings n) as bs
exprBindings n (ETuple { eContents = as }) (ETuple { eContents = bs }) =
    concat $ zipWith (exprBindings n) as bs
exprBindings n (EMacro { eExpr = a }) (EMacro { eExpr = b }) =
    exprBindings n a b
exprBindings n (EParticle { eParticle = ap' }) (EParticle { eParticle = bp }) =
    case (ap', bp) of
        (Keyword { mNames = _, mTargets = ames }, Keyword { mNames = _, mTargets = bmes }) ->
            concatMap (\(Just a, Just b) -> exprBindings n a b)
            $ filter (isJust . fst)
            $ zip ames bmes
        _ -> []
exprBindings n (EQuote { eExpr = a }) (EQuote { eExpr = b }) =
    exprBindings (n + 1) a b
exprBindings _ _ _ = []

-- | Convert an expression to the pattern match it represents.
toPattern :: Expr -> Maybe Pattern
toPattern (EDispatch { eMessage = Keyword { mNames = ["."], mTargets = [h, t] } }) = do
    hp <- toPattern h
    tp <- toPattern t
    return (PHeadTail hp tp)
toPattern (EDispatch { eMessage = Keyword { mNames = ["->"], mTargets = [ETop {}, o] } }) = do
    liftM PInstance (toPattern o)
toPattern (EDispatch { eMessage = Keyword { mNames = ["=="], mTargets = [ETop {}, o] } }) = do
    liftM PStrict (toPattern o)
toPattern (EDispatch { eMessage = Keyword { mNames = ["..."], mTargets = [ETop {}, o] } }) = do
    liftM PVariable (toPattern o)
toPattern (EDispatch { eMessage = Keyword { mNames = [n], mTargets = [ETop {}, x] } }) = do
    p <- toPattern x
    return (PNamed n p)
toPattern (EDispatch { eMessage = Keyword { mNames = ns, mTargets = ts } }) =
    return (pkeyword ns (map PObject ts))
toPattern (EDispatch { eMessage = Single { mName = "_" } }) =
    return PAny
toPattern (EDispatch { eMessage = Single { mName = n, mTarget = ETop {} } }) =
    return (PNamed n PAny)
toPattern (EDispatch { eMessage = Single { mTarget = d@(EDispatch {}), mName = n } }) =
    return (psingle n (PObject d))
toPattern (EList { eContents = es }) = do
    ps <- mapM toPattern es
    return (PList ps)
toPattern (ETuple { eContents = es }) = do
    ps <- mapM toPattern es
    return (PTuple ps)
toPattern (EParticle { eParticle = Single { mName = n } }) =
    return (PMatch (particle n))
toPattern (EParticle { eParticle = Keyword { mNames = ns, mTargets = mes } }) = do
    ps <- forM mes $ \me ->
        case me of
            Nothing -> return PAny
            Just e -> toPattern e

    return (PPMKeyword ns ps)
toPattern (EQuote { eExpr = e }) = return (PExpr e)
toPattern (EPrimitive { eValue = v }) =
    return (PMatch v)
toPattern (ETop {}) =
    return (PObject (ETop Nothing))
toPattern b@(EBlock {}) =
    return (PObject (EDispatch Nothing (keyword ["call-in"] [b, ETop Nothing])))
toPattern _ = Nothing

-- | Convert an expression into a definition's message pattern.
toDefinePattern :: Expr -> Maybe (Message Pattern)
toDefinePattern (EDispatch { eMessage = Single { mName = n, mTarget = t, mOptionals = os } }) = do
    p <- toRolePattern t
    return (single' n p (map (\(Option oi on oe) -> Option oi on (PObject oe)) os))
toDefinePattern (EDispatch { eMessage = Keyword { mNames = ns, mTargets = ts, mOptionals = os } }) = do
    ps <- mapM toRolePattern ts
    return (keyword' ns ps (map (\(Option oi on oe) -> Option oi on (PObject oe)) os))
toDefinePattern _ = Nothing

-- | Convert an expression into a pattern-match for use as a message's role.
toRolePattern :: Expr -> Maybe Pattern
toRolePattern (EDispatch { eMessage = Keyword { mNames = ["->"], mTargets = [ETop {}, o] } }) = do
    liftM PInstance (toRolePattern o)
toRolePattern (EDispatch { eMessage = Keyword { mNames = ["=="], mTargets = [ETop {}, o] } }) = do
    liftM PStrict (toRolePattern o)
toRolePattern (EDispatch { eMessage = Keyword { mNames = ["..."], mTargets = [ETop {}, o] } }) = do
    liftM PVariable (toPattern o)
toRolePattern (EDispatch { eMessage = Keyword { mNames = [n], mTargets = [ETop {}, x] } }) = do
    p <- toRolePattern x
    return (PNamed n p)
toRolePattern d@(EDispatch { eMessage = Single { mTarget = ETop {}, mName = n } })
    | isUpper (head n) = return (PObject d)
    | n == "_" = return PAny
    | otherwise = return (PNamed n PAny)
toRolePattern d@(EDispatch { eMessage = Single { mTarget = (EDispatch {}) } }) =
    return (PObject d)
toRolePattern p = toPattern p

-- | Convert an expression into a macro's message pattern.
toMacroPattern :: Expr -> Maybe (Message Pattern)
toMacroPattern (EDispatch { eMessage = Single { mName = n, mTarget = t, mOptionals = os } }) = do
    p <- toMacroRole t
    return (single' n p (map macroOptional os))
toMacroPattern (EDispatch { eMessage = Keyword { mNames = ns, mTargets = ts, mOptionals = os } }) = do
    ps <- mapM toMacroRole ts
    return (keyword' ns ps (map macroOptional os))
toMacroPattern _ = Nothing

macroOptional :: Option Expr -> Option Pattern
macroOptional (Option i n e) = Option i n (PExpr e)

-- | Convert an expression into a pattern-match for use as a macro's role.
toMacroRole :: Expr -> Maybe Pattern
toMacroRole (EDispatch _ (Single { mName = "Dispatch" })) = Just PEDispatch
toMacroRole (EDispatch _ (Single { mName = "Operator" })) = Just PEOperator
toMacroRole (EDispatch _ (Single { mName = "Primitive" })) = Just PEPrimitive
toMacroRole (EDispatch _ (Single { mName = "Block" })) = Just PEBlock
toMacroRole (EDispatch _ (Single { mName = "List" })) = Just PEList
toMacroRole (EDispatch _ (Single { mName = "Tuple" })) = Just PETuple
toMacroRole (EDispatch _ (Single { mName = "Macro" })) = Just PEMacro
toMacroRole (EDispatch _ (Single { mName = "ForMacro" })) = Just PEForMacro
toMacroRole (EDispatch _ (Single { mName = "Particle" })) = Just PEParticle
toMacroRole (EDispatch _ (Single { mName = "Top" })) = Just PETop
toMacroRole (EDispatch _ (Single { mName = "Quote" })) = Just PEQuote
toMacroRole (EDispatch _ (Single { mName = "Unquote" })) = Just PEUnquote
toMacroRole (EDispatch _ (Single { mName = "MacroQuote" })) = Just PEMacroQuote
toMacroRole (EDispatch { eMessage = Keyword { mNames = [n], mTargets = [ETop {}, x] } }) = do
    p <- toMacroRole x
    return (PNamed n p)
toMacroRole (ETop {}) = Just PAny
toMacroRole p = toPattern p
