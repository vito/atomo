{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.List (load) where

import Data.IORef
import qualified Data.Vector as V

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    [$p|(l: List) length|] =:
        getList [$e|l|] >>= return . Integer . fromIntegral . V.length

    [$p|(l: List) empty?|] =:
        getList [$e|l|] >>= bool . V.null

    [$p|(l: List) at: (n: Integer)|] =: do
        Integer n <- here "n" >>= findValue isInteger
        vs <- getList [$e|l|]
        return (vs V.! fromIntegral n)

    [$p|(l: List) head|] =:
        getList [$e|l|] >>= return . V.head

    [$p|(l: List) last|] =:
        getList [$e|l|] >>= return . V.last

    [$p|(l: List) from: (s: Integer) to: (e: Integer)|] =: do
        vs <- getList [$e|l|]
        Integer start <- here "s" >>= findValue isInteger
        Integer end <- here "e" >>= findValue isInteger
        list' (V.slice (fromIntegral start) (fromIntegral end) vs)

    [$p|(l: List) init|] =:
        getList [$e|l|] >>= list' . V.init

    [$p|(l: List) tail|] =:
        getList [$e|l|] >>= list' . V.tail

    [$p|(l: List) take: (n: Integer)|] =: do
        vs <- getList [$e|l|]
        Integer n <- here "n" >>= findValue isInteger
        list' (V.take (fromIntegral n) vs)

    [$p|(l: List) drop: (n: Integer)|] =: do
        vs <- getList [$e|l|]
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
        as <- getList [$e|a|]
        bs <- getList [$e|b|]
        list' (as V.++ bs)

    [$p|(l: List) reverse|] =: do
        getList [$e|l|] >>= list' . V.reverse

    [$p|(l: List) map: b|] =: do
        vs <- getList [$e|l|]
        b <- here "b"

        nvs <- V.mapM (\v -> do
            as <- list' (V.singleton v)
            dispatch (keyword ["call"] [b, as])) vs

        list' nvs

    -- TODO: zip

    [$p|(x: List) zip: (y: List) with: b|] =: do
        xs <- getList [$e|x|]
        ys <- getList [$e|y|]
        b <- here "b"

        nvs <- V.zipWithM (\x y -> do
            as <- list' (V.fromList [x, y])
            dispatch (keyword ["call"] [b, as])) xs ys

        list' nvs

    [$p|(l: List) filter: b|] =: do
        vs <- getList [$e|l|]
        b <- here "b"

        t <- bool True
        nvs <- V.filterM (\v -> do
            as <- list' (V.singleton v)
            check <- dispatch (keyword ["call"] [b, as])
            return (check == t)) vs

        list' nvs

    [$p|(l: List) reduce: b|] =: do
        vs <- getList [$e|l|]
        b <- here "b"

        V.fold1M (\x acc -> do
            as <- list [x, acc]
            dispatch (keyword ["call"] [b, as])) vs

    [$p|(l: List) reduce: b with: v|] =: do
        vs <- getList [$e|l|]
        b <- here "b"
        v <- here "v"

        V.foldM (\x acc -> do
            as <- list [x, acc]
            dispatch (keyword ["call"] [b, as])) v vs

    [$p|(l: List) concat|] =::: [$e|l reduce: @.. with: []|]
    [$p|(l: List) sum|] =::: [$e|l reduce: @+ with: 0|]
    [$p|(l: List) product|] =::: [$e|l reduce: @* with: 1|]
    [$p|(l: List) maximum|] =::: [$e|l reduce: @max:|]
    [$p|(l: List) minimum|] =::: [$e|l reduce: @min:|]

    [$p|(l: List) all?: b|] =: do
        vs <- getList [$e|l|]
        b <- here "b"

        t <- bool True
        nvs <- V.mapM (\v -> do
            as <- list' (V.singleton v)
            check <- dispatch (keyword ["call"] [b, as])
            return (check == t)) vs

        bool (V.and nvs)

    [$p|(l: List) any?: b|] =: do
        vs <- getList [$e|l|]
        b <- here "b"

        t <- bool True
        nvs <- V.mapM (\v -> do
            as <- list' (V.singleton v)
            check <- dispatch (keyword ["call"] [b, as])
            return (check == t)) vs

        bool (V.or nvs)

    [$p|(l: List) and|] =: do
        vs <- getList [$e|l|]
        t <- bool True
        bool (V.all (== t) vs)

    [$p|(l: List) or|] =: do
        vs <- getList [$e|l|]
        t <- bool True
        bool (V.any (== t) vs)

    -- TODO: take-while, drop-while

    [$p|(l: List) contains?: v|] =::: [$e|l any?: @(== v)|]

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
        vs <- getList [$e|l|]

        Integer n <- here "n" >>= findValue isInteger
        v <- here "v"

        liftIO . writeIORef l $ vs V.// [(fromIntegral n, v)]

        return (List l)

    [$p|v . (l: List)|] =: do
        vs <- getList [$e|l|]
        v <- here "v"
        l <- liftIO . newIORef $ V.cons v vs
        return (List l)

    [$p|(l: List) << v|] =::: [$e|l push: v|]
    [$p|v >> (l: List)|] =::: [$e|l left-push: v|]

    [$p|(l: List) push: v|] =: do
        List l <- here "l" >>= findValue isList
        vs <- getList [$e|l|]
        v <- here "v"

        liftIO . writeIORef l $ V.snoc vs v

        return (List l)

    [$p|(l: List) left-push: v|] =: do
        List l <- here "l" >>= findValue isList
        vs <- getList [$e|l|]
        v <- here "v"

        liftIO . writeIORef l $ V.cons v vs

        return (List l)
