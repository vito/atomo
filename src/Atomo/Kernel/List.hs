{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.List (load) where

import Data.IORef
import Data.List.Split
import qualified Data.Vector as V

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    eval [$e|operator right .|]

    [$p|(l: List) length|] =:
        getVector [$e|l|] >>= return . Integer . fromIntegral . V.length

    [$p|(l: List) empty?|] =:
        getVector [$e|l|] >>= bool . V.null

    [$p|(l: List) at: (n: Integer)|] =: do
        Integer n <- here "n" >>= findValue isInteger
        vs <- getVector [$e|l|]
        return (vs V.! fromIntegral n)

    [$p|(l: List) head|] =:
        getVector [$e|l|] >>= return . V.head

    [$p|(l: List) last|] =:
        getVector [$e|l|] >>= return . V.last

    [$p|(l: List) from: (s: Integer) to: (e: Integer)|] =: do
        vs <- getVector [$e|l|]
        Integer start <- here "s" >>= findValue isInteger
        Integer end <- here "e" >>= findValue isInteger
        list' (V.slice (fromIntegral start) (fromIntegral end) vs)

    [$p|(l: List) init|] =:
        getVector [$e|l|] >>= list' . V.init

    [$p|(l: List) tail|] =:
        getVector [$e|l|] >>= list' . V.tail

    [$p|(l: List) take: (n: Integer)|] =: do
        vs <- getVector [$e|l|]
        Integer n <- here "n" >>= findValue isInteger
        list' (V.take (fromIntegral n) vs)

    [$p|(l: List) drop: (n: Integer)|] =: do
        vs <- getVector [$e|l|]
        Integer n <- here "n" >>= findValue isInteger
        list' (V.drop (fromIntegral n) vs)

    [$p|v replicate: (n: Integer)|] =: do
        v <- here "v"
        Integer n <- here "n" >>= findValue isInteger
        list' (V.replicate (fromIntegral n) v)

    [$p|b repeat: (n: Integer)|] =: do
        b <- here "b"
        Integer n <- here "n" >>= findValue isInteger
        vs <- V.replicateM (fromIntegral n) $
            dispatch (single "call" b)
        list' vs

    [$p|(a: List) .. (b: List)|] =: do
        as <- getVector [$e|a|]
        bs <- getVector [$e|b|]
        list' (as V.++ bs)

    [$p|(l: List) reverse|] =: do
        getVector [$e|l|] >>= list' . V.reverse

    [$p|(l: List) map: b|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"

        nvs <- V.mapM (\v -> do
            as <- list' (V.singleton v)
            dispatch (keyword ["call"] [b, as])) vs

        list' nvs

    [$p|(x: List) zip: (y: List)|] =::: [$e|x zip: y with: @->|]
    [$p|(x: List) zip: (y: List) with: b|] =: do
        xs <- getVector [$e|x|]
        ys <- getVector [$e|y|]
        b <- here "b"

        nvs <- V.zipWithM (\x y -> do
            as <- list [x, y]
            dispatch (keyword ["call"] [b, as])) xs ys

        list' nvs

    [$p|(l: List) filter: b|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"

        t <- bool True
        nvs <- V.filterM (\v -> do
            as <- list [v]
            check <- dispatch (keyword ["call"] [b, as])
            return (check == t)) vs

        list' nvs

    [$p|(l: List) reduce: b|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"

        V.fold1M (\x acc -> do
            as <- list [x, acc]
            dispatch (keyword ["call"] [b, as])) vs

    [$p|(l: List) reduce: b with: v|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"
        v <- here "v"

        V.foldM (\x acc -> do
            as <- list [x, acc]
            dispatch (keyword ["call"] [b, as])) v vs

    [$p|(l: List) reduce-right: b|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"

        if V.null vs
            then throwError (ErrorMsg "@reduce-right: empty list")
            else do

        foldr1MV (\x acc -> do
            as <- list [x, acc]
            dispatch (keyword ["call"] [b, as])) vs

    [$p|(l: List) reduce-right: b with: v|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"
        v <- here "v"

        foldrMV (\x acc -> do
            as <- list [x, acc]
            dispatch (keyword ["call"] [b, as])) v vs

    [$p|(l: List) concat|] =::: [$e|l reduce: @.. with: []|]
    [$p|(l: List) sum|] =::: [$e|l reduce: @+ with: 0|]
    [$p|(l: List) product|] =::: [$e|l reduce: @* with: 1|]
    [$p|(l: List) maximum|] =::: [$e|l reduce: @max:|]
    [$p|(l: List) minimum|] =::: [$e|l reduce: @min:|]

    [$p|(l: List) all?: b|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"

        t <- bool True
        nvs <- V.mapM (\v -> do
            as <- list' (V.singleton v)
            check <- dispatch (keyword ["call"] [b, as])
            return (check == t)) vs

        bool (V.and nvs)

    [$p|(l: List) any?: b|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"

        t <- bool True
        nvs <- V.mapM (\v -> do
            as <- list' (V.singleton v)
            check <- dispatch (keyword ["call"] [b, as])
            return (check == t)) vs

        bool (V.or nvs)

    [$p|(l: List) and|] =: do
        vs <- getVector [$e|l|]
        t <- bool True
        bool (V.all (== t) vs)

    [$p|(l: List) or|] =: do
        vs <- getVector [$e|l|]
        t <- bool True
        bool (V.any (== t) vs)

    -- TODO: take-while, drop-while

    [$p|(l: List) contains?: v|] =::: [$e|l any?: @(== v)|]
    [$p|v in?: (l: List)|] =::: [$e|l contains?: v|]

    -- TODO: find

    [$p|(x: Integer) .. (y: Integer)|] =: do
        Integer x <- here "x" >>= findValue isInteger
        Integer y <- here "y" >>= findValue isInteger

        if x < y
            then dispatch (keyword ["up-to"] [Integer x, Integer y])
            else dispatch (keyword ["down-to"] [Integer x, Integer y])

    [$p|(x: Integer) ... (y: Integer)|] =: do
        Integer x <- here "x" >>= findValue isInteger
        Integer y <- here "y" >>= findValue isInteger

        if x < y
            then dispatch (keyword ["up-to"] [Integer x, Integer (y - 1)])
            else dispatch (keyword ["down-to"] [Integer x, Integer (y + 1)])

    [$p|(x: Integer) to: (y: Integer) by: (d: Integer)|] =: do
        Integer x <- here "x" >>= findValue isInteger
        Integer y <- here "y" >>= findValue isInteger
        Integer d <- here "d" >>= findValue isInteger

        list' $ V.generate
            (fromIntegral $ abs ((y - x) `div` d) + 1)
            (Integer . (x +) . (* d) . fromIntegral)

    [$p|(x: Integer) up-to: (y: Integer)|] =::: [$e|x to: y by: 1|]
    [$p|(x: Integer) down-to: (y: Integer)|] =::: [$e|x to: y by: -1|]

    -- destructive update
    [$p|(l: List) at: (n: Integer) put: v|] =: do
        List l <- here "l" >>= findValue isList
        vs <- getVector [$e|l|]

        Integer n <- here "n" >>= findValue isInteger
        v <- here "v"

        liftIO . writeIORef l $ vs V.// [(fromIntegral n, v)]

        return (List l)

    [$p|v . (l: List)|] =: do
        vs <- getVector [$e|l|]
        v <- here "v"
        l <- liftIO . newIORef $ V.cons v vs
        return (List l)

    [$p|(l: List) << v|] =::: [$e|l push: v|]
    [$p|v >> (l: List)|] =::: [$e|l left-push: v|]

    [$p|(l: List) push: v|] =: do
        List l <- here "l" >>= findValue isList
        vs <- getVector [$e|l|]
        v <- here "v"

        liftIO . writeIORef l $ V.snoc vs v

        return (List l)

    [$p|(l: List) left-push: v|] =: do
        List l <- here "l" >>= findValue isList
        vs <- getVector [$e|l|]
        v <- here "v"

        liftIO . writeIORef l $ V.cons v vs

        return (List l)

    [$p|(l: List) split: (d: List)|] =: do
        l <- getList [$e|l|]
        d <- getList [$e|d|]

        mapM list (splitOn d l) >>= list

    [$p|(l: List) split-on: d|] =: do
        l <- getList [$e|l|]
        d <- here "d"

        mapM list (splitWhen (== d) l) >>= list

    [$p|(l: List) sort|] =:
        getList [$e|l|] >>= sortVM >>= list

    [$p|(l: List) sort-by: cmp|] =: do
        t <- bool True
        vs <- getList [$e|l|]
        cmp <- here "cmp"

        sortByVM (\a b -> do
            as <- list [a, b]
            r <- dispatch (keyword ["call"] [cmp, as])
            return (r == t)) vs >>= list

    prelude


prelude :: VM ()
prelude = mapM_ eval [$es|
    (l: List) each: (b: Block) := {
        l map: b in-context
        l
    } call

    [] includes?: List := False
    (x: List) includes?: (y: List) :=
        if: (x (take: y length) == y)
            then: { True }
            else: { x tail includes?: y }

    [] join: List := []
    [x] join: List := x
    (x . xs) join: (d: List) :=
        x .. d .. (xs join: d)
|]

foldr1MV :: (Value -> Value -> VM Value) -> V.Vector Value -> VM Value
foldr1MV f vs = foldrMV f (V.last vs) (V.init vs)

foldrMV :: (Value -> Value -> VM Value) -> Value -> V.Vector Value -> VM Value
foldrMV _ acc vs | V.null vs = return acc
foldrMV f acc vs = do
    rest <- foldrMV f acc (V.tail vs)
    f (V.head vs) rest

sortVM :: [Value] -> VM [Value]
sortVM = sortByVM gt
  where
    gt a b = do
        t <- bool True
        r <- dispatch (keyword [">"] [a, b])
        return (r == t)

sortByVM :: (Value -> Value -> VM Bool) -> [Value] -> VM [Value]
sortByVM = mergesort

mergesort :: (Value -> Value -> VM Bool) -> [Value] -> VM [Value]
mergesort cmp = mergesort' cmp . map (\x -> [x])

mergesort' :: (Value -> Value -> VM Bool) -> [[Value]] -> VM [Value]
mergesort' _ [] = return []
mergesort' _ [xs] = return xs
mergesort' cmp xss = merge_pairs cmp xss >>= mergesort' cmp

merge_pairs :: (Value -> Value -> VM Bool) -> [[Value]] -> VM [[Value]]
merge_pairs _ [] = return []
merge_pairs _ [xs] = return [xs]
merge_pairs cmp (xs:ys:xss) = do
    z <- merge cmp xs ys
    zs <- merge_pairs cmp xss
    return (z:zs)

merge :: (Value -> Value -> VM Bool) -> [Value] -> [Value] -> VM [Value]
merge _ [] ys = return ys
merge _ xs [] = return xs
merge cmp (x:xs) (y:ys) = do
    o <- cmp x y

    if o
        then do
            rest <- merge cmp (x:xs) ys
            return (y:rest)
        else do
            rest <- merge cmp xs (y:ys)
            return (x:rest)

