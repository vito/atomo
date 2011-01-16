module Atomo.Method
    ( addMethod
    , elemsMap
    , emptyMap
    , insertMethod
    , insertMap
    , lookupMap
    , memberMap
    , noMethods
    , nullMap
    , toMethods
    ) where

import Data.List (elemIndices)
import Data.Maybe (isJust)
import qualified Data.IntMap as M

import Atomo.Types


-- referring to the left side:
--   LT = is higher-precision
--   GT = is lower-precision
comparePrecision :: Pattern -> Pattern -> Ordering
comparePrecision (PNamed _ a) (PNamed _ b) =
    comparePrecision a b
comparePrecision (PNamed _ a) b = comparePrecision a b
comparePrecision a (PNamed _ b) = comparePrecision a b
comparePrecision PAny PAny = EQ
comparePrecision PThis PThis = EQ
comparePrecision (PMatch a@(Object {})) (PMatch b@(Object {}))
    | delegatesTo a b = LT
    | delegatesTo a b = GT
    | otherwise = EQ
comparePrecision (PMatch _) (PMatch _) = EQ
comparePrecision (PList as) (PList bs) =
    comparePrecisions as bs
comparePrecision (PPMKeyword _ as) (PPMKeyword _ bs) =
    comparePrecisions as bs
comparePrecision (PHeadTail ah at) (PHeadTail bh bt) =
    comparePrecisions [ah, at] [bh, bt]
comparePrecision (PMessage (Single { mTarget = at })) (PMessage (Single { mTarget = bt })) =
    comparePrecision at bt
comparePrecision (PMessage (Keyword { mTargets = as })) (PMessage (Keyword { mTargets = bs })) =
    compareHeads as bs
comparePrecision (PObject _) (PObject _) = EQ
comparePrecision PAny _ = GT
comparePrecision _ PAny = LT
comparePrecision PThis (PMatch (Object {})) = LT
comparePrecision (PMatch (Object {})) PThis = GT
comparePrecision (PMatch _) _ = LT
comparePrecision _ (PMatch _) = GT
comparePrecision (PExpr a) (PExpr b) = exprPrecision 0 a b
comparePrecision (PExpr _) _ = LT
comparePrecision _ (PExpr _) = GT
comparePrecision (PList _) _ = LT
comparePrecision _ (PList _) = GT
comparePrecision (PPMKeyword _ _) _ = LT
comparePrecision _ (PPMKeyword _ _) = GT
comparePrecision (PHeadTail _ _) _ = LT
comparePrecision _ (PHeadTail _ _) = GT
comparePrecision PThis _ = LT
comparePrecision _ PThis = GT
comparePrecision (PObject _) _ = LT
comparePrecision _ (PObject _) = GT
comparePrecision _ _ = GT

compareHeads :: [Pattern] -> [Pattern] -> Ordering
compareHeads [a] [b] = comparePrecision a b
compareHeads (a:as) (b:bs) =
    case comparePrecision a b of
        EQ -> compareHeads as bs
        x -> x
compareHeads a b = error $ "impossible: compareHeads on " ++ show (a, b)

comparePrecisions :: [Pattern] -> [Pattern] -> Ordering
comparePrecisions = comparePrecisionsWith comparePrecision

comparePrecisionsWith :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
comparePrecisionsWith cmp as bs =
    compare gt lt
  where
    compared = zipWith cmp as bs
    gt = length $ elemIndices GT compared
    lt = length $ elemIndices LT compared

delegatesTo :: Value -> Value -> Bool
delegatesTo (Object { oDelegates = ds }) t =
    t `elem` ds || any (`delegatesTo` t) ds
delegatesTo _ _ = False

exprPrecision :: Int -> Expr -> Expr -> Ordering
exprPrecision 0 (EUnquote {}) (EUnquote {}) = EQ
exprPrecision 0 (EUnquote {}) _ = GT
exprPrecision 0 _ (EUnquote {}) = LT
exprPrecision n (EDefine { eExpr = a }) (EDefine { eExpr = b }) =
    exprPrecision n a b
exprPrecision n (ESet { eExpr = a }) (ESet { eExpr = b }) =
    exprPrecision n a b
exprPrecision n (EDispatch { eMessage = am@(Keyword {}) }) (EDispatch { eMessage = bm@(Keyword {}) }) =
    comparePrecisionsWith (exprPrecision n) (mTargets am) (mTargets bm)
exprPrecision n (EDispatch { eMessage = am@(Single {}) }) (EDispatch { eMessage = bm@(Single {}) }) =
    exprPrecision n (mTarget am) (mTarget bm)
exprPrecision n (EBlock { eContents = as }) (EBlock { eContents = bs }) =
    comparePrecisionsWith (exprPrecision n) as bs
exprPrecision n (EList { eContents = as }) (EList { eContents = bs }) =
    comparePrecisionsWith (exprPrecision n) as bs
exprPrecision n (EMacro { eExpr = a }) (EMacro { eExpr = b }) =
    exprPrecision n a b
exprPrecision n (EParticle { eParticle = ap' }) (EParticle { eParticle = bp }) =
    case (ap', bp) of
        (Keyword { mTargets = ames }, Keyword { mTargets = bmes }) ->
            comparePrecisionsWith (exprPrecision n) (firsts ames bmes) (seconds ames bmes)
        _ -> EQ
  where
    pairs ames bmes = map (\(Just a, Just b) -> (a, b)) $ filter (\(a, b) -> isJust a && isJust b) $ zip ames bmes

    firsts ames = fst . unzip . pairs ames
    seconds ames = fst . unzip . pairs ames
exprPrecision n (EQuote { eExpr = a }) (EQuote { eExpr = b }) =
    exprPrecision (n + 1) a b
exprPrecision _ _ _ = EQ


-- | Insert a method into a MethodMap based on its pattern's ID and precision.
addMethod :: Method -> MethodMap -> MethodMap
addMethod m mm =
    M.insertWith (\[m'] ms -> insertMethod m' ms) key [m] mm
  where
    key = mID $ mPattern m

-- | Insert a method into a list of existing methods most precise goes first,
-- equivalent patterns are replaced.
insertMethod :: Method -> [Method] -> [Method]
insertMethod x [] = [x]
insertMethod x (y:ys)
    | mPattern x == mPattern y = x : ys
    | otherwise =
        case comparePrecision (PMessage (mPattern x)) (PMessage (mPattern y)) of
            -- stop at LT so it's after all of the definitons before this one
            LT -> x : y : ys

            -- keep looking if we're EQ or GT
            _ -> y : insertMethod x ys

-- | Convert a list of slots to a MethodMap.
toMethods :: [(Message Pattern, Value)] -> MethodMap
toMethods = foldl (\ss (p, v) -> addMethod (Slot p v) ss) emptyMap

-- | A pair of two empty MethodMaps; one for single methods and one for keyword
-- methods.
noMethods :: (MethodMap, MethodMap)
noMethods = (M.empty, M.empty)

-- | An empty MethodMap.
emptyMap :: MethodMap
emptyMap = M.empty

-- | Find methods in a MethodMap by the pattern ID.
lookupMap :: Int -> MethodMap -> Maybe [Method]
lookupMap = M.lookup

-- | Is a MethodMap empty?.
nullMap :: MethodMap -> Bool
nullMap = M.null

-- | All of the methods in a MethodMap.
elemsMap :: MethodMap -> [[Method]]
elemsMap = M.elems

-- | Is a key set in a map?
memberMap :: Int -> MethodMap -> Bool
memberMap = M.member


-- | Insert a method into a MethodMap, replacing all other methods with the
-- same ID.
insertMap :: Method -> MethodMap -> MethodMap
insertMap m mm = M.insert key [m] mm
  where
    key = mID $ mPattern m
