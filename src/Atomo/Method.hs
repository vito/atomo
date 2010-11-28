module Atomo.Method
    ( addMethod
    , elemsMap
    , emptyMap
    , insertMethod
    , insertMap
    , lookupMap
    , noMethods
    , nullMap
    , toMethods
    ) where

import Data.IORef
import Data.List (elemIndices)
import System.IO.Unsafe
import qualified Data.IntMap as M

import Atomo.Types


-- referring to the left side:
--   LT = higher-precision
--   GT = lower-precision
comparePrecision :: Pattern -> Pattern -> Ordering
comparePrecision (PNamed _ a) (PNamed _ b) =
    comparePrecision a b
comparePrecision (PNamed _ a) b = comparePrecision a b
comparePrecision a (PNamed _ b) = comparePrecision a b
comparePrecision PAny PAny = EQ
comparePrecision PThis PThis = EQ
comparePrecision (PMatch (Reference a)) (PMatch (Reference b))
    | unsafeDelegatesTo (Reference a) (Reference b) = LT
    | unsafeDelegatesTo (Reference a) (Reference b) = GT
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
comparePrecision PThis (PMatch (Reference _)) = LT
comparePrecision (PMatch (Reference _)) PThis = GT
comparePrecision (PMatch _) _ = LT
comparePrecision _ (PMatch _) = GT
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
comparePrecisions as bs =
    compare gt lt
  where
    compared = zipWith comparePrecision as bs
    gt = length $ elemIndices GT compared
    lt = length $ elemIndices LT compared

unsafeDelegatesTo :: Value -> Value -> Bool
unsafeDelegatesTo (Reference f) t =
    t `elem` ds || any (`unsafeDelegatesTo` t) ds
  where
    ds = oDelegates (unsafePerformIO (readIORef f))
unsafeDelegatesTo _ _ = False

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

-- | Insert a method into a MethodMap, replacing all other methods with the
-- same ID.
insertMap :: Method -> MethodMap -> MethodMap
insertMap m mm = M.insert key [m] mm
  where
    key = mID $ mPattern m
