module Atomo.Method (addMethod, insertMethod, toMethods) where

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
comparePrecision (PPMSingle _) (PPMSingle _) = EQ
comparePrecision (PPMKeyword _ as) (PPMKeyword _ bs) =
    comparePrecisions as bs
comparePrecision (PHeadTail ah at) (PHeadTail bh bt) =
    comparePrecisions [ah, at] [bh, bt]
comparePrecision (PSingle { ppTarget = at }) (PSingle { ppTarget = bt }) =
    comparePrecision at bt
comparePrecision (PKeyword { ppTargets = as }) (PKeyword { ppTargets = bs }) =
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
comparePrecision (PPMSingle _) _ = LT
comparePrecision _ (PPMSingle _) = GT
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
    t `elem` ds || any (flip unsafeDelegatesTo t) ds
  where
    ds = oDelegates (unsafePerformIO (readIORef f))
unsafeDelegatesTo _ _ = False

addMethod :: Method -> MethodMap -> MethodMap
addMethod m mm =
    M.insertWith (\[m'] ms -> insertMethod m' ms) key [m] mm
  where
    key = ppID (mPattern m)

-- insert a method into a list of existing methods
-- most precise goes first, equivalent patterns are replaced
insertMethod :: Method -> [Method] -> [Method]
insertMethod x [] = [x]
insertMethod x ys@(y:ys') =
    case comparePrecision (mPattern x) (mPattern y) of
        -- stop at LT so it's after all of the definitons before this one
        LT -> x : ys

        -- replace equivalent patterns
        _ | equivalent (mPattern x) (mPattern y) -> insertMethod x ys'

        -- keep looking if we're EQ or GT
        _ -> y : insertMethod x ys'

toMethods :: [(Pattern, Value)] -> MethodMap
toMethods bs = foldl (\ss (p, v) -> addMethod (Slot p v) ss) M.empty bs

-- check if two patterns are "equivalent", ignoring names for patterns
-- and other things that mean the same thing
equivalent :: Pattern -> Pattern -> Bool
equivalent PAny PAny = True
equivalent (PHeadTail ah at) (PHeadTail bh bt) =
    equivalent ah bh && equivalent at bt
equivalent (PKeyword _ ans aps) (PKeyword _ bns bps) =
    ans == bns && and (zipWith equivalent aps bps)
equivalent (PList aps) (PList bps) =
    length aps == length bps && and (zipWith equivalent aps bps)
equivalent (PMatch a) (PMatch b) = a == b
equivalent (PNamed _ a) (PNamed _ b) = equivalent a b
equivalent (PNamed _ a) b = equivalent a b
equivalent a (PNamed _ b) = equivalent a b
equivalent (PPMSingle a) (PPMSingle b) = a == b
equivalent (PPMKeyword ans aps) (PPMKeyword bns bps) =
    ans == bns && and (zipWith equivalent aps bps)
equivalent (PSingle ai _ at) (PSingle bi _ bt) =
    ai == bi && equivalent at bt
equivalent PThis PThis = True
equivalent _ _ = False
