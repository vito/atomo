{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.String where

import Data.List (sort)
import qualified Data.Text as T

import Atomo


load :: VM ()
load = do
    [$p|(s: String) as: List|] =:
        liftM (list . map Char) (getString [$e|s|])

    [$p|(s: String) as: Char|] =: do
        s <- getString [$e|s|]
        return (Char (head s))
    
    [$p|(s: String) as: Integer|] =: do
        s <- getString [$e|s|]
        return (Integer (read s))
        
    [$p|(s: String) as: Double|] =: do
        s <- getString [$e|s|]
        return (Double (read s))
    
    [$p|(s: String) as: Rational|] =: do
        s <- getString [$e|s|]
        let num = read $ takeWhile (/= '/') s
            denom = read . tail $ dropWhile (/= '/') s
        return (Rational (num % denom))

    [$p|(l: List) to-string|] =: do
        vs <- getList [$e|l|]

        if all isChar vs
            then return $ string (map (\(Char c) -> c) vs)
            else raise' "list-not-homogenous"

    [$p|(c: Char) singleton|] =: do
        Char c <- here "c" >>= findChar
        return (String (T.singleton c))

    [$p|(s: String) length|] =:
        liftM (Integer . fromIntegral . T.length) (getText [$e|s|])

    [$p|(s: String) empty?|] =:
        liftM (Boolean . T.null) $ getText [$e|s|]

    [$p|(s: String) at: (n: Integer)|] =: do
        Integer n <- here "n" >>= findInteger
        t <- getText [$e|s|]

        if fromIntegral n >= T.length t
            then raise ["out-of-bounds", "for-string"] [Integer n, String t]
            else return . Char $ t `T.index` fromIntegral n

    [$p|"" head|] =::: [$e|error: @empty-string|]
    [$p|(s: String) head|] =:
        liftM (Char . T.head) (getText [$e|s|])

    [$p|"" last|] =::: [$e|error: @empty-string|]
    [$p|(s: String) last|] =:
        liftM (Char . T.last) (getText [$e|s|])

    [$p|(s: String) from: (n: Integer) to: (m: Integer)|] =: do
            Integer n <- here "n" >>= findInteger
            Integer m <- here "m" >>= findInteger
            t <- getText [$e|s|]

            let start = fromIntegral n
                count = (fromIntegral m) - start

            if count > T.length t || start < 0 || count < 0
                then raise
                    ["invalid-slice", "for-string"]
                    [keyParticleN ["from", "to"] [Integer n, Integer m], String t]
                else return (String . T.take count . T.drop start $ t)

    [$p|"" init|] =::: [$e|error: @empty-string|]
    [$p|(s: String) init|] =:
        liftM (String . T.init) (getText [$e|s|])

    [$p|"" tail|] =::: [$e|error: @empty-string|]
    [$p|(s: String) tail|] =:
        liftM (String . T.tail) (getText [$e|s|])

    [$p|(s: String) take: (n: Integer)|] =: do
        Integer n <- here "n" >>= findInteger
        liftM (String . T.take (fromIntegral n)) (getText [$e|s|])

    [$p|(s: String) drop: (n: Integer)|] =: do
        Integer n <- here "n" >>= findInteger
        liftM (String . T.drop (fromIntegral n)) (getText [$e|s|])

    [$p|(s: String) take-while: test|] =: do
        t <- here "test"
        s <- getString [$e|s|]

        let takeWhileM [] = return []
            takeWhileM (x:xs) =
                ifVM (dispatch (keyword ["call"] [t, list [Char x]]))
                    (liftM (x:) (takeWhileM xs))
                    (return [])

        liftM string $ takeWhileM s

    [$p|(s: String) drop-while: test|] =: do
        t <- here "test"
        s <- getString [$e|s|]

        let dropWhileM [] = return []
            dropWhileM (x:xs) =
                ifVM (dispatch (keyword ["call"] [t, list [Char x]]))
                    (dropWhileM xs)
                    (return (x:xs))

        liftM string $ dropWhileM s

    [$p|(c: Char) repeat: (n: Integer)|] =: do
        Char c <- here "c" >>= findChar
        Integer n <- here "n" >>= findInteger
        return (string (replicate (fromIntegral n) c))

    [$p|(s: String) repeat: (n: Integer)|] =: do
        Integer n <- here "n" >>= findInteger
        liftM (String . T.replicate (fromIntegral n)) (getText [$e|s|])

    [$p|(a: String) .. (b: String)|] =: do
        a <- getText [$e|a|]
        b <- getText [$e|b|]
        return (String (a `T.append` b))

    [$p|(s: String) reverse|] =:
        liftM (String . T.reverse) (getText [$e|s|])

    [$p|(l: List) join|] =::: [$e|l reduce: @.. with: ""|]

    [$p|(l: List) join: (d: String)|] =: do
        ts <- getList [$e|l|]
            >>= mapM (liftM (\(String t) -> t) . findString)

        d <- getText [$e|d|]

        return (String (T.intercalate d ts))

    [$p|(s: String) intersperse: (c: Char)|] =: do
        Char c <- here "c" >>= findChar
        t <- getText [$e|s|]
        return (String (T.intersperse c t))

    [$p|(s: String) split: (d: String)|] =: do
        s <- getText [$e|s|]
        d <- getText [$e|d|]
        return $ list (map String (T.split d s))

    -- TODO: split-by

    [$p|(s: String) split-on: (d: Char)|] =: do
        s <- getText [$e|s|]
        Char d <- here "d" >>= findChar
        return $ list (map String (T.splitBy (== d) s))

    [$p|(s: String) split-at: (n: Integer)|] =: do
        Integer n <- here "n" >>= findInteger
        s <- getText [$e|s|]
        let (a, b) = T.splitAt (fromIntegral n) s
        return $ list [String a, String b]

    [$p|(s: String) break-on: (d: Integer)|] =: do
        s <- getText [$e|s|]
        d <- getText [$e|d|]
        let (a, b) = T.break d s
        return $ list [String a, String b]

    [$p|(s: String) break-end: (d: Integer)|] =: do
        s <- getText [$e|s|]
        d <- getText [$e|d|]
        let (a, b) = T.breakEnd d s
        return $ list [String a, String b]

    [$p|(s: String) group|] =: do
        s <- getText [$e|s|]
        return $ list (map String (T.group s))

    [$p|(s: String) inits|] =: do
        s <- getText [$e|s|]
        return $ list (map String (T.inits s))

    [$p|(s: String) tails|] =: do
        s <- getText [$e|s|]
        return $ list (map String (T.tails s))

    [$p|(s: String) chunks-of: (n: Integer)|] =: do
        Integer n <- here "n" >>= findInteger
        s <- getText [$e|s|]
        return $ list (map String (T.chunksOf (fromIntegral n) s))

    [$p|(s: String) lines|] =: do
        s <- getText [$e|s|]
        return $ list (map String (T.lines s))

    [$p|(s: String) words|] =: do
        s <- getText [$e|s|]
        return $ list (map String (T.words s))

    [$p|(l: List) unlines|] =: do
        l <- getList [$e|l|]
        return $ String (T.unlines (map fromString l))

    [$p|(l: List) unwords|] =: do
        l <- getList [$e|l|]
        return $ String (T.unwords (map fromString l))

    [$p|(s: String) map: b|] =: do
        s <- getString [$e|s|]
        b <- here "b"

        vs <- forM s $ \c ->
            dispatch (keyword ["call"] [b, list [Char c]])

        if all isChar vs
            then return (string (map (\(Char c) -> c) vs))
            else return $ list vs

    [$p|(s: String) each: (b: Block)|] =::: [$e|{ s map: b in-context; s } call|]

    [$p|(c: Char) . (s: String)|] =: do
        Char c <- here "c" >>= findChar
        s <- getText [$e|s|]
        return (String (T.cons c s))

    [$p|(c: Char) >> (s: String)|] =::: [$e|c . s|]

    [$p|(s: String) << (c: Char)|] =: do
        s <- getText [$e|s|]
        Char c <- here "c" >>= findChar
        return (String (T.snoc s c))

    [$p|(haystack: String) replace: (needle: String) with: (new: String)|] =: do
        h <- getText [$e|haystack|]
        n <- getText [$e|needle|]
        s <- getText [$e|new|]
        return (String (T.replace n s h))

    [$p|(s: String) case-fold|] =:
        liftM (String . T.toCaseFold) (getText [$e|s|])

    [$p|(s: String) lowercase|] =:
        liftM (String . T.toLower) (getText [$e|s|])

    [$p|(s: String) uppercase|] =:
        liftM (String . T.toUpper) (getText [$e|s|])

    [$p|(s: String) left-justify: (length: Integer) with: (c: Char)|] =: do
        s <- getText [$e|s|]
        Integer l <- here "length" >>= findInteger
        Char c <- here "c" >>= findChar

        return (String (T.justifyLeft (fromIntegral l) c s))

    [$p|(s: String) right-justify: (length: Integer) with: (c: Char)|] =: do
        s <- getText [$e|s|]
        Integer l <- here "length" >>= findInteger
        Char c <- here "c" >>= findChar

        return (String (T.justifyRight (fromIntegral l) c s))

    [$p|(s: String) center: (length: Integer) with: (c: Char)|] =: do
        s <- getText [$e|s|]
        Integer l <- here "length" >>= findInteger
        Char c <- here "c" >>= findChar

        return (String (T.center (fromIntegral l) c s))

    [$p|(s: String) strip|] =:
        liftM (String . T.strip) (getText [$e|s|])

    [$p|(s: String) strip-start|] =:
        liftM (String . T.stripStart) (getText [$e|s|])

    [$p|(s: String) strip-end|] =:
        liftM (String . T.stripEnd) (getText [$e|s|])

    [$p|(s: String) strip: (c: Char)|] =: do
        Char c <- here "c" >>= findChar
        liftM (String . T.dropAround (== c)) (getText [$e|s|])

    [$p|(s: String) strip-start: (c: Char)|] =: do
        Char c <- here "c" >>= findChar
        liftM (String . T.dropWhile (== c)) (getText [$e|s|])

    [$p|(s: String) strip-end: (c: Char)|] =: do
        Char c <- here "c" >>= findChar
        liftM (String . T.dropWhileEnd (== c)) (getText [$e|s|])

    [$p|(s: String) all?: b|] =::: [$e|(s as: List) all?: b|]
    [$p|(s: String) any?: b|] =::: [$e|(s as: List) any?: b|]

    [$p|(s: String) contains?: (c: Char)|] =: do
        t <- getText [$e|s|]
        Char c <- here "c" >>= findChar
        return (Boolean (T.any (== c) t))

    [$p|(c: Char) in?: (s: String)|] =::: [$e|s contains?: c|]

    [$p|(s: String) reduce: b|] =::: [$e|(s as: List) reduce: b|]
    [$p|(s: String) reduce: b with: v|] =::: [$e|(s as: List) reduce: b with: v|]

    [$p|(s: String) reduce-right: b|] =::: [$e|(s as: List) reduce-right: b|]
    [$p|(s: String) reduce-right: b with: v|] =::: [$e|(s as: List) reduce-right: b with: v|]

    [$p|(s: String) maximum|] =:
        liftM (Char . T.maximum) (getText [$e|s|])

    [$p|(s: String) minimum|] =:
        liftM (Char . T.minimum) (getText [$e|s|])

    [$p|(s: String) sort|] =:
        liftM (string . sort) (getString [$e|s|])

    [$p|(s: String) sort-by: cmp|] =::: [$e|s (as: List) (sort-by: cmp) to-string|]

    [$p|(a: String) is-prefix-of?: (b: String)|] =: do
        a <- getText [$e|a|]
        b <- getText [$e|b|]
        return $ Boolean (a `T.isPrefixOf` b)

    [$p|(a: String) is-suffix-of?: (b: String)|] =: do
        a <- getText [$e|a|]
        b <- getText [$e|b|]
        return $ Boolean (a `T.isSuffixOf` b)

    [$p|(a: String) is-infix-of?: (b: String)|] =: do
        a <- getText [$e|a|]
        b <- getText [$e|b|]
        return $ Boolean (a `T.isInfixOf` b)

    [$p|(a: String) starts-with?: (b: String)|] =::: [$e|b is-prefix-of?: a|]
    [$p|(a: String) ends-with?: (b: String)|] =::: [$e|b is-suffix-of?: a|]
    [$p|(a: String) includes?: (b: String)|] =::: [$e|b is-infix-of?: a|]

    [$p|(s: String) filter: b|] =::: [$e|s (as: List) (filter: b) to-string|]

    [$p|(x: String) zip: (y: String)|] =::: [$e|x zip: y with: @->|]
    [$p|(x: String) zip: (y: String) with: z|] =: do
        x <- getText [$e|x|]
        y <- getText [$e|y|]
        z <- here "z"

        vs <- forM (T.zip x y) $ \(a, b) ->
            dispatch (keyword ["call"] [z, list [Char a, Char b]])

        return $ list vs

    [$p|(x: List) zip: (y: String)|] =::: [$e|x zip: (y as: List)|]
    [$p|(x: String) zip: (y: List)|] =::: [$e|(x as: List) zip: y|]

    [$p|(x: List) zip: (y: String) with: z|] =:::
        [$e|x zip: (y as: List) with: z|]

    [$p|(x: String) zip: (y: List) with: z|] =:::
        [$e|(x as: List) zip: y with: z|]
