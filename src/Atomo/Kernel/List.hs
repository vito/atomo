{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.List (load) where

import Data.List.Split
import qualified Data.Vector as V

import Atomo


load :: VM ()
load = do
    eval [$e|operator right .|]

    [$p|(l: List) show|] =:::
        [$e|"[" .. l (map: @show) (join: ", ") .. "]"|]

    [$p|(l: List) length|] =:
        getVector [$e|l|] >>= return . Integer . fromIntegral . V.length

    [$p|(l: List) empty?|] =:
        liftM (Boolean . V.null) $ getVector [$e|l|]

    [$p|(l: List) at: (n: Integer)|] =: do
        Integer n <- here "n" >>= findInteger
        vs <- getVector [$e|l|]

        if fromIntegral n >= V.length vs
            then here "l" >>= \l -> raise
                ["out-of-bounds", "for-list"]
                [ Integer n
                , l
                ]
            else return (vs `V.unsafeIndex` fromIntegral n)

    [$p|[] head|] =::: [$e|raise: @empty-list|]
    [$p|(l: List) head|] =:
        getVector [$e|l|] >>= return . V.unsafeHead

    [$p|[] last|] =::: [$e|raise: @empty-list|]
    [$p|(l: List) last|] =:
        getVector [$e|l|] >>= return . V.unsafeLast

    -- TODO: handle negative ranges
    [$p|(l: List) from: (s: Integer) to: (e: Integer)|] =:::
        [$e|l from: s take: (e - s)|]

    [$p|(l: List) from: (s: Integer) take: (n: Integer)|] =: do
        vs <- getVector [$e|l|]
        Integer start <- here "s" >>= findInteger
        Integer num <- here "n" >>= findInteger

        if start < 0 || num < 0 || (start + num) > fromIntegral (V.length vs)
            then here "l" >>= \l -> raise
                ["invalid-slice", "for-list"]
                [ keyParticleN ["from", "take"] [Integer start, Integer num]
                , l
                ]
            else return . List $ V.unsafeSlice
                (fromIntegral start)
                (fromIntegral num)
                vs

    [$p|[] init|] =::: [$e|raise: @empty-list|]
    [$p|(l: List) init|] =:
        getVector [$e|l|] >>= return . List . V.unsafeInit

    [$p|[] tail|] =::: [$e|raise: @empty-list|]
    [$p|(l: List) tail|] =:
        getVector [$e|l|] >>= return . List . V.unsafeTail

    [$p|(l: List) take: (n: Integer)|] =: do
        vs <- getVector [$e|l|]
        Integer n <- here "n" >>= findInteger
        return . List $ V.take (fromIntegral n) vs

    [$p|(l: List) drop: (n: Integer)|] =: do
        vs <- getVector [$e|l|]
        Integer n <- here "n" >>= findInteger
        return . List $ V.drop (fromIntegral n) vs

    [$p|v replicate: (n: Integer)|] =: do
        v <- here "v"
        Integer n <- here "n" >>= findInteger
        return . List $ V.replicate (fromIntegral n) v

    [$p|b repeat: (n: Integer)|] =: do
        b <- here "b"
        Integer n <- here "n" >>= findInteger
        vs <- V.replicateM (fromIntegral n) $
            dispatch (single "call" b)
        return $ List vs

    [$p|(a: List) .. (b: List)|] =: do
        as <- getVector [$e|a|]
        bs <- getVector [$e|b|]
        return . List $ as V.++ bs

    [$p|(l: List) reverse|] =: do
        getVector [$e|l|] >>= return . List . V.reverse

    [$p|(l: List) map: b|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"

        nvs <- V.mapM (\v ->
            dispatch (keyword ["call"] [b, list [v]])) vs

        return $ List nvs

    [$p|(x: List) zip: (y: List)|] =::: [$e|x zip: y with: @->|]
    [$p|(x: List) zip: (y: List) with: b|] =: do
        xs <- getVector [$e|x|]
        ys <- getVector [$e|y|]
        b <- here "b"

        nvs <- V.zipWithM (\x y ->
            dispatch (keyword ["call"] [b, list [x, y]])) xs ys

        return $ List nvs

    [$p|(l: List) filter: b|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"

        nvs <- V.filterM (\v -> do
            Boolean t <- dispatch (keyword ["call"] [b, list [v]]) >>= findBoolean
            return t) vs

        return $ List nvs

    [$p|[] reduce: b|] =::: [$e|raise: @empty-list|]
    [$p|(l: List) reduce: b|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"

        V.fold1M (\x acc ->
            dispatch (keyword ["call"] [b, list [x, acc]])) vs

    [$p|(l: List) reduce: b with: v|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"
        v <- here "v"

        V.foldM (\x acc ->
            dispatch (keyword ["call"] [b, list [x, acc]])) v vs

    [$p|[] reduce-right: b|] =::: [$e|raise: @empty-list|]
    [$p|(l: List) reduce-right: b|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"

        foldr1MV (\x acc ->
            dispatch (keyword ["call"] [b, list [x, acc]])) vs

    [$p|(l: List) reduce-right: b with: v|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"
        v <- here "v"

        foldrMV (\x acc ->
            dispatch (keyword ["call"] [b, list [x, acc]])) v vs

    [$p|(l: List) concat|] =::: [$e|l reduce: @.. with: []|]
    [$p|(l: List) sum|] =::: [$e|l reduce: @+ with: 0|]
    [$p|(l: List) product|] =::: [$e|l reduce: @* with: 1|]
    [$p|(l: List) maximum|] =::: [$e|l reduce: @max:|]
    [$p|(l: List) minimum|] =::: [$e|l reduce: @min:|]

    [$p|(l: List) all?: b|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"

        nvs <- V.mapM (\v -> do
            Boolean t <- dispatch (keyword ["call"] [b, list [v]]) >>= findBoolean
            return t) vs

        return $ Boolean (V.and nvs)

    [$p|(l: List) any?: b|] =: do
        vs <- getVector [$e|l|]
        b <- here "b"

        nvs <- V.mapM (\v -> do
            Boolean t <- dispatch (keyword ["call"] [b, list [v]]) >>= findBoolean
            return t) vs

        return $ Boolean (V.or nvs)

    [$p|(l: List) and|] =::: [$e|l all?: @(== True)|]
    [$p|(l: List) or|] =::: [$e|l any?: @(== True)|]

    -- TODO: take-while, drop-while

    [$p|v in?: (l: List)|] =::: [$e|l contains?: v|]
    [$p|(l: List) contains?: v|] =::: [$e|l any?: @(== v)|]

    -- TODO: find

    [$p|(x: Integer) .. (y: Integer)|] =: do
        Integer x <- here "x" >>= findInteger
        Integer y <- here "y" >>= findInteger

        if x < y
            then dispatch (keyword ["up-to"] [Integer x, Integer y])
            else dispatch (keyword ["down-to"] [Integer x, Integer y])

    [$p|(x: Integer) ... (y: Integer)|] =: do
        Integer x <- here "x" >>= findInteger
        Integer y <- here "y" >>= findInteger

        if x < y
            then dispatch (keyword ["up-to"] [Integer x, Integer (y - 1)])
            else dispatch (keyword ["down-to"] [Integer x, Integer (y + 1)])

    [$p|(x: Integer) to: (y: Integer) by: (d: Integer)|] =: do
        Integer x <- here "x" >>= findInteger
        Integer y <- here "y" >>= findInteger
        Integer d <- here "d" >>= findInteger

        return . List $ V.generate
            (fromIntegral $ abs ((y - x) `div` d) + 1)
            (Integer . (x +) . (* d) . fromIntegral)

    [$p|(x: Integer) up-to: (y: Integer)|] =::: [$e|x to: y by: 1|]
    [$p|(x: Integer) down-to: (y: Integer)|] =::: [$e|x to: y by: -1|]

    -- destructive update
    [$p|(l: List) at: (n: Integer) put: v|] =: do
        vs <- getVector [$e|l|]

        Integer n <- here "n" >>= findInteger
        v <- here "v"

        if fromIntegral n >= V.length vs
            then here "l" >>= \l' -> raise
                ["out-of-bounds", "for-list"]
                [ Integer n
                , l'
                ]
            else do

        return (List $ vs V.// [(fromIntegral n, v)])

    [$p|v . (l: List)|] =: do
        vs <- getVector [$e|l|]
        v <- here "v"
        return (List $ V.cons v vs)

    [$p|(l: List) << v|] =: do
        vs <- getVector [$e|l|]
        v <- here "v"

        return . List $ V.snoc vs v

    [$p|v >> (l: List)|] =: do
        vs <- getVector [$e|l|]
        v <- here "v"

        return . List $ V.cons v vs

    {-[$p|[] pop!|] =::: [$e|raise: @empty-list|]-}
    {-[$p|(l: List) pop!|] =: do-}
        {-List l <- here "l" >>= findList-}
        {-vs <- getVector [$e|l|]-}

        {-return . List l $ V.tail vs-}

    [$p|(l: List) split: (d: List)|] =: do
        l <- getList [$e|l|]
        d <- getList [$e|d|]

        return $ list (map list (splitOn d l))

    [$p|(l: List) split-on: d|] =: do
        l <- getList [$e|l|]
        d <- here "d"

        return $ list (map list (splitWhen (== d) l))

    [$p|(l: List) sort|] =:
        getList [$e|l|] >>= liftM list . sortVM

    [$p|(l: List) sort-by: cmp|] =: do
        vs <- getList [$e|l|]
        cmp <- here "cmp"

        liftM list $ sortByVM (\a b -> do
            Boolean t <- dispatch (keyword ["call"] [cmp, list [a, b]]) >>= findBoolean
            return t) vs

    prelude


prelude :: VM ()
prelude = mapM_ eval [$es|
    (l: List) each: (b: Block) :=
      { l map: b in-context
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
        Boolean t <- dispatch (keyword [">"] [a, b]) >>= findBoolean
        return t

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

