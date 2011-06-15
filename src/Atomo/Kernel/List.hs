{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.List (load) where

import Data.List (isPrefixOf)
import qualified Data.Vector as V

import Atomo


load :: VM ()
load = do
    eval [e|operator right .|]

    [p|List new: (... contents)|] =::: [e|contents|]

    [p|(l: List) length|] =:
        liftM (Integer . fromIntegral . V.length) (getVector [e|l|])

    [p|(l: List) empty?|] =:
        liftM (Boolean . V.null) (getVector [e|l|])

    [p|(l: List) at: (n: Integer)|] =: do
        Integer n <- here "n" >>= findInteger
        vs <- getVector [e|l|]

        if fromIntegral n >= V.length vs
            then here "l" >>= \l -> raise
                ["out-of-bounds", "for-list"]
                [ Integer n
                , l
                ]
            else return (vs `V.unsafeIndex` fromIntegral n)

    [p|[] head|] =::: [e|error: @empty-list|]
    [p|(l: List) head|] =:
        liftM V.unsafeHead (getVector [e|l|])

    [p|[] last|] =::: [e|error: @empty-list|]
    [p|(l: List) last|] =:
        liftM V.unsafeLast (getVector [e|l|])

    -- TODO: handle negative ranges
    [p|(l: List) from: (s: Integer) to: (e: Integer)|] =:::
        [e|l from: s take: (e - s)|]

    [p|(l: List) from: (s: Integer) take: (n: Integer)|] =: do
        vs <- getVector [e|l|]
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

    [p|[] init|] =::: [e|error: @empty-list|]
    [p|(l: List) init|] =:
        liftM (List . V.unsafeInit) (getVector [e|l|])

    [p|[] tail|] =::: [e|error: @empty-list|]
    [p|(l: List) tail|] =:
        liftM (List . V.unsafeTail) (getVector [e|l|])

    [p|(l: List) take: (n: Integer)|] =: do
        vs <- getVector [e|l|]
        Integer n <- here "n" >>= findInteger
        return . List $ V.take (fromIntegral n) vs

    [p|(l: List) drop: (n: Integer)|] =: do
        vs <- getVector [e|l|]
        Integer n <- here "n" >>= findInteger
        return . List $ V.drop (fromIntegral n) vs

    [p|v replicate: (n: Integer)|] =: do
        v <- here "v"
        Integer n <- here "n" >>= findInteger
        return . List $ V.replicate (fromIntegral n) v

    [p|(a: List) .. (b: List)|] =: do
        as <- getVector [e|a|]
        bs <- getVector [e|b|]
        return . List $ as V.++ bs

    [p|(l: List) reverse|] =:
        liftM (List . V.reverse) (getVector [e|l|])

    [p|(l: List) map: b|] =: do
        vs <- getVector [e|l|]
        b <- here "b"

        nvs <- V.mapM (\v ->
            dispatch (keyword ["call"] [b, v])) vs

        return $ List nvs

    [p|(x: List) zip: (... ys) &zipper: @id|] =: do
        xs <- getVector [e|x|]
        yss <- getList [e|ys|] >>= mapM (liftM (\(List v) -> v) . findList)
        z <- here "zipper"

        let zipped = zipN (xs:yss)

        if z == particle "id"
            then return (list (map list zipped))
            else liftM list $ forM zipped $ \vs ->
                dispatch (keyword ["call"] [z, tuple vs])

    [p|(l: List) filter: b|] =: do
        vs <- getVector [e|l|]
        b <- here "b"

        nvs <- V.filterM (\v -> do
            Boolean t <- dispatch (keyword ["call"] [b, v]) >>= findBoolean
            return t) vs

        return $ List nvs

    [p|[] reduce: b|] =::: [e|error: @empty-list|]
    [p|(l: List) reduce: b|] =: do
        vs <- getVector [e|l|]
        b <- here "b"

        V.fold1M (\x acc ->
            dispatch (keyword ["call"] [b, tuple [x, acc]])) vs

    [p|(l: List) reduce: b with: v|] =: do
        vs <- getVector [e|l|]
        b <- here "b"
        v <- here "v"

        V.foldM (\x acc ->
            dispatch (keyword ["call"] [b, tuple [x, acc]])) v vs

    [p|[] reduce-right: b|] =::: [e|error: @empty-list|]
    [p|(l: List) reduce-right: b|] =: do
        vs <- getVector [e|l|]
        b <- here "b"

        foldr1MV (\x acc ->
            dispatch (keyword ["call"] [b, tuple [x, acc]])) vs

    [p|(l: List) reduce-right: b with: v|] =: do
        vs <- getVector [e|l|]
        b <- here "b"
        v <- here "v"

        foldrMV (\x acc ->
            dispatch (keyword ["call"] [b, tuple [x, acc]])) v vs

    [p|(l: List) concat|] =::: [e|l reduce: @.. with: []|]
    [p|(l: List) sum|] =::: [e|l reduce: @+ with: 0|]
    [p|(l: List) product|] =::: [e|l reduce: @* with: 1|]
    [p|(l: List) maximum|] =::: [e|l reduce: @max:|]
    [p|(l: List) minimum|] =::: [e|l reduce: @min:|]

    [p|(l: List) all?: b|] =: do
        vs <- getVector [e|l|]
        b <- here "b"

        nvs <- V.mapM (\v -> do
            Boolean t <- dispatch (keyword ["call"] [b, v]) >>= findBoolean
            return t) vs

        return $ Boolean (V.and nvs)

    [p|(l: List) any?: b|] =: do
        vs <- getVector [e|l|]
        b <- here "b"

        nvs <- V.mapM (\v -> do
            Boolean t <- dispatch (keyword ["call"] [b, v]) >>= findBoolean
            return t) vs

        return $ Boolean (V.or nvs)

    [p|(l: List) and|] =::: [e|l all?: @(== True)|]
    [p|(l: List) or|] =::: [e|l any?: @(== True)|]

    [p|(l: List) take-while: test|] =: do
        t <- here "test"
        l <- getList [e|l|]

        let takeWhileM [] = return []
            takeWhileM (x:xs) =
                ifVM (dispatch (keyword ["call"] [t, x]))
                    (liftM (x:) (takeWhileM xs))
                    (return [])

        liftM list $ takeWhileM l

    [p|(l: List) drop-while: test|] =: do
        t <- here "test"
        l <- getList [e|l|]

        let dropWhileM [] = return []
            dropWhileM (x:xs) =
                ifVM (dispatch (keyword ["call"] [t, x]))
                    (dropWhileM xs)
                    (return (x:xs))

        liftM list $ dropWhileM l

    [p|v in?: (l: List)|] =::: [e|l contains?: v|]
    [p|(l: List) contains?: v|] =::: [e|l any?: @(== v)|]

    -- TODO: find

    [p|(l: List) at: (n: Integer) put: v|] =: do
        vs <- getVector [e|l|]

        Integer n <- here "n" >>= findInteger
        v <- here "v"

        if fromIntegral n >= V.length vs
            then here "l" >>= \l' -> raise
                ["out-of-bounds", "for-list"]
                [ Integer n
                , l'
                ]
            else return (List $ vs V.// [(fromIntegral n, v)])

    [p|v . (l: List)|] =: do
        vs <- getVector [e|l|]
        v <- here "v"
        return (List $ V.cons v vs)

    [p|(l: List) push: v|] =: do
        vs <- getVector [e|l|]
        v <- here "v"

        return . List $ V.snoc vs v

    [p|(l: List) cons: v|] =: do
        vs <- getVector [e|l|]
        v <- here "v"

        return . List $ V.cons v vs

    [p|(l: List) split: (d: List)|] =: do
        l <- getList [e|l|]
        d <- getList [e|d|]

        return $ list (map list (splitOn d l))

    [p|(l: List) split-on: d|] =: do
        l <- getList [e|l|]
        d <- here "d"

        return $ list (map list (splitWhen (== d) l))

    [p|(l: List) sort &comparison: @<=>|] =: do
        cmp <- here "comparison"
        getList [e|l|] >>= liftM list . sortVM cmp

    [p|(l: List) sort-by: something &comparison: @<=>|] =: do
        vs <- getList [e|l|]
        something <- here "something"
        cmp <- here "comparison"

        liftM list $ sortByVM cmp something vs


foldr1MV :: (Value -> Value -> VM Value) -> V.Vector Value -> VM Value
foldr1MV f vs = foldrMV f (V.last vs) (V.init vs)

foldrMV :: (Value -> Value -> VM Value) -> Value -> V.Vector Value -> VM Value
foldrMV _ acc vs | V.null vs = return acc
foldrMV f acc vs = do
    rest <- foldrMV f acc (V.tail vs)
    f (V.head vs) rest

sortVM :: Value -> [Value] -> VM [Value]
sortVM cmp = mergesort gt
  where
    gt a b = do
        Integer t <- dispatch (keyword ["call"] [cmp, tuple [a, b]])
            >>= findInteger
        return t

sortByVM :: Value -> Value -> [Value] -> VM [Value]
sortByVM cmp by = mergesort gt
  where
    gt a b = do
        x <- dispatch (keyword ["call"] [by, a])
        y <- dispatch (keyword ["call"] [by, b])
        Integer t <- dispatch (keyword ["call"] [cmp, tuple [x, y]])
            >>= findInteger
        return t

mergesort :: (Value -> Value -> VM Integer) -> [Value] -> VM [Value]
mergesort cmp = mergesort' cmp . map (: [])

mergesort' :: (Value -> Value -> VM Integer) -> [[Value]] -> VM [Value]
mergesort' _ [] = return []
mergesort' _ [xs] = return xs
mergesort' cmp xss = mergePairs cmp xss >>= mergesort' cmp

mergePairs :: (Value -> Value -> VM Integer) -> [[Value]] -> VM [[Value]]
mergePairs _ [] = return []
mergePairs _ [xs] = return [xs]
mergePairs cmp (xs:ys:xss) = do
    z <- merge cmp xs ys
    zs <- mergePairs cmp xss
    return (z:zs)

merge :: (Value -> Value -> VM Integer) -> [Value] -> [Value] -> VM [Value]
merge _ [] ys = return ys
merge _ xs [] = return xs
merge cmp (x:xs) (y:ys) = do
    o <- cmp x y

    if o > 0
        then do
            rest <- merge cmp (x:xs) ys
            return (y:rest)
        else do
            rest <- merge cmp xs (y:ys)
            return (x:rest)

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f vs' = splitWhen' vs' []
  where
    splitWhen' [] acc = [acc]
    splitWhen' (v:vs) acc
        | f v = acc : splitWhen' vs []
        | otherwise = splitWhen' vs (acc ++ [v])

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn d vs' = splitOn' vs' []
  where
    splitOn' [] acc = [acc]
    splitOn' a@(v:vs) acc
        | d `isPrefixOf` a = acc : splitOn' (drop (length d) a) []
        | otherwise = splitOn' vs (acc ++ [v])

zipN :: [VVector] -> [[Value]]
zipN vs
    | any V.null vs = []
    | otherwise = (map V.head vs) : zipN (map V.tail vs)
