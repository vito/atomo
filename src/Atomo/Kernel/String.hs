{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.String where

import Data.List (sort)
import qualified Data.Vector as V
import qualified Data.Text as T

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    [$p|(s: String) as: List|] =:
        getString [$e|s|] >>= list . map Char

    [$p|(l: List) to-string|] =: do
        vs <- fmap V.toList (getList [$e|l|])

        if all isChar vs
            then return $ string (map (\(Char c) -> c) vs)
            else throwError $ ErrorMsg "@as:String - list must contain only Chars"

    [$p|(c: Char) singleton|] =: do
        Char c <- here "c" >>= findValue isChar
        return (String (T.singleton c))

    [$p|(s: String) length|] =:
        getText [$e|s|] >>= return . Integer . fromIntegral . T.length

    [$p|(s: String) empty?|] =:
        getText [$e|s|] >>= bool . T.null

    [$p|(s: String) at: (n: Integer)|] =: do
        Integer n <- here "n" >>= findValue isInteger
        t <- getText [$e|s|]
        return . Char $ t `T.index` fromIntegral n

    [$p|"" head|] =: throwError (ErrorMsg "head: empty string")
    [$p|(s: String) head|] =:
        getText [$e|s|] >>= return . Char . T.head

    [$p|"" last|] =: throwError (ErrorMsg "last: empty string")
    [$p|(s: String) last|] =:
        getText [$e|s|] >>= return . Char . T.last

    -- TODO: @from:to:

    [$p|"" init|] =: throwError (ErrorMsg "init: empty string")
    [$p|(s: String) init|] =:
        getText [$e|s|] >>= return . String . T.init

    [$p|"" tail|] =: throwError (ErrorMsg "tail: empty string")
    [$p|(s: String) tail|] =:
        getText [$e|s|] >>= return . String . T.tail

    [$p|(s: String) take: (n: Integer)|] =: do
        Integer n <- here "n" >>= findValue isInteger
        getText [$e|s|] >>=
            return . String . T.take (fromIntegral n)

    [$p|(s: String) drop: (n: Integer)|] =: do
        Integer n <- here "n" >>= findValue isInteger
        getText [$e|s|] >>=
            return . String . T.drop (fromIntegral n)

    -- TODO: take-while:, drop-while:

    [$p|(c: Char) repeat: (n: Integer)|] =: do
        Char c <- here "c" >>= findValue isChar
        Integer n <- here "n" >>= findValue isInteger
        return (string (replicate (fromIntegral n) c))

    [$p|(s: String) repeat: (n: Integer)|] =: do
        Integer n <- here "n" >>= findValue isInteger
        getText [$e|s|] >>=
            return . String . T.replicate (fromIntegral n)

    [$p|(a: String) .. (b: String)|] =: do
        a <- getText [$e|a|]
        b <- getText [$e|b|]
        return (String (a `T.append` b))

    [$p|(s: String) reverse|] =:
        getText [$e|s|] >>= return . String . T.reverse

    [$p|(l: List) join|] =::: [$e|l reduce: @.. with: ""|]

    [$p|(l: List) join: (d: String)|] =: do
        ts <- fmap V.toList (getList [$e|l|])
            >>= mapM (fmap (\(String t) -> t) . findValue isString)

        d <- getText [$e|d|]

        return (String (T.intercalate d ts))

    [$p|(s: String) intersperse: (c: Char)|] =: do
        Char c <- here "c" >>= findValue isChar
        t <- getText [$e|s|]
        return (String (T.intersperse c t))

    [$p|(s: String) split: (d: String)|] =: do
        s <- getText [$e|s|]
        d <- getText [$e|d|]
        list (map String (T.split d s))

    -- TODO: split-by

    [$p|(s: String) split-on: (d: Char)|] =: do
        s <- getText [$e|s|]
        Char d <- here "d" >>= findValue isChar
        list (map String (T.splitBy (== d) s))

    [$p|(s: String) split-at: (n: Integer)|] =: do
        Integer n <- here "n" >>= findValue isInteger
        s <- getText [$e|s|]
        let (a, b) = T.splitAt (fromIntegral n) s
        list [String a, String b]

    [$p|(s: String) break-on: (d: Integer)|] =: do
        s <- getText [$e|s|]
        d <- getText [$e|d|]
        let (a, b) = T.break d s
        list [String a, String b]

    [$p|(s: String) break-end: (d: Integer)|] =: do
        s <- getText [$e|s|]
        d <- getText [$e|d|]
        let (a, b) = T.breakEnd d s
        list [String a, String b]

    [$p|(s: String) group|] =: do
        s <- getText [$e|s|]
        list (map String (T.group s))

    [$p|(s: String) inits|] =: do
        s <- getText [$e|s|]
        list (map String (T.inits s))

    [$p|(s: String) tails|] =: do
        s <- getText [$e|s|]
        list (map String (T.tails s))

    [$p|(s: String) chunks-of: (n: Integer)|] =: do
        Integer n <- here "n" >>= findValue isInteger
        s <- getText [$e|s|]
        list (map String (T.chunksOf (fromIntegral n) s))

    [$p|(s: String) lines|] =: do
        s <- getText [$e|s|]
        list (map String (T.lines s))

    [$p|(s: String) words|] =: do
        s <- getText [$e|s|]
        list (map String (T.words s))

    [$p|(l: List) unlines|] =::: [$e|l (map: @(<< '\n')) join|]
    [$p|(l: List) unwords|] =::: [$e|l join: " "|]

    [$p|(s: String) map: b|] =: do
        s <- getString [$e|s|]
        b <- here "b"

        vs <- forM s $ \c -> do
            as <- list [Char c]
            dispatch (keyword ["call"] [b, as])

        if all isChar vs
            then return (string (map (\(Char c) -> c) vs))
            else list vs

    [$p|(s: String) each: (b: Block)|] =::: [$e|{ s map: b in-context; s } call|]

    [$p|(c: Char) . (s: String)|] =: do
        Char c <- here "c" >>= findValue isChar
        s <- getText [$e|s|]
        return (String (T.cons c s))

    [$p|(s: String) << (c: Char)|] =: do
        s <- getText [$e|s|]
        Char c <- here "c" >>= findValue isChar
        return (String (T.snoc s c))

    [$p|(haystack: String) replace: (needle: String) with: (new: String)|] =: do
        h <- getText [$e|haystack|]
        n <- getText [$e|needle|]
        s <- getText [$e|new|]
        return (String (T.replace n s h))

    [$p|(s: String) case-fold|] =: do
        getText [$e|s|] >>= return . String . T.toCaseFold

    [$p|(s: String) lowercase|] =: do
        getText [$e|s|] >>= return . String . T.toLower

    [$p|(s: String) uppercase|] =: do
        getText [$e|s|] >>= return . String . T.toUpper

    [$p|(s: String) left-justify: (length: Integer) with: (c: Char)|] =: do
        s <- getText [$e|s|]
        Integer l <- here "length" >>= findValue isInteger
        Char c <- here "c" >>= findValue isChar

        return (String (T.justifyLeft (fromIntegral l) c s))

    [$p|(s: String) right-justify: (length: Integer) with: (c: Char)|] =: do
        s <- getText [$e|s|]
        Integer l <- here "length" >>= findValue isInteger
        Char c <- here "c" >>= findValue isChar

        return (String (T.justifyRight (fromIntegral l) c s))

    [$p|(s: String) center: (length: Integer) with: (c: Char)|] =: do
        s <- getText [$e|s|]
        Integer l <- here "length" >>= findValue isInteger
        Char c <- here "c" >>= findValue isChar

        return (String (T.center (fromIntegral l) c s))

    [$p|(s: String) strip|] =: do
        getText [$e|s|] >>= return . String . T.strip

    [$p|(s: String) strip-start|] =: do
        getText [$e|s|] >>= return . String . T.stripStart

    [$p|(s: String) strip-end|] =: do
        getText [$e|s|] >>= return . String . T.stripEnd

    [$p|(s: String) strip: (c: Char)|] =: do
        Char c <- here "c" >>= findValue isChar
        getText [$e|s|] >>= return . String . T.dropAround (== c)

    [$p|(s: String) strip-start: (c: Char)|] =: do
        Char c <- here "c" >>= findValue isChar
        getText [$e|s|] >>= return . String . T.dropWhile (== c)

    [$p|(s: String) strip-end: (c: Char)|] =: do
        Char c <- here "c" >>= findValue isChar
        getText [$e|s|] >>= return . String . T.dropWhileEnd (== c)

    [$p|(s: String) all?: b|] =::: [$e|(s as: List) all?: b|]
    [$p|(s: String) any?: b|] =::: [$e|(s as: List) any?: b|]

    [$p|(s: String) contains?: (c: Char)|] =: do
        t <- getText [$e|s|]
        Char c <- here "c" >>= findValue isChar
        bool (T.any (== c) t)

    [$p|(c: Char) in?: (s: String)|] =::: [$e|s contains?: c|]

    [$p|(s: String) reduce: b|] =::: [$e|(s as: List) reduce: b|]
    [$p|(s: String) reduce: b with: v|] =::: [$e|(s as: List) reduce: b with: v|]

    [$p|(s: String) reduce-right: b|] =::: [$e|(s as: List) reduce-right: b|]
    [$p|(s: String) reduce-right: b with: v|] =::: [$e|(s as: List) reduce-right: b with: v|]

    [$p|(s: String) maximum|] =: do
        getText [$e|s|] >>= return . Char . T.maximum

    [$p|(s: String) minimum|] =: do
        getText [$e|s|] >>= return . Char . T.minimum

    [$p|(s: String) sort|] =:
        getString [$e|s|] >>= return . string . sort

    [$p|(s: String) sort-by: cmp|] =::: [$e|s (as: List) (sort-by: cmp) to-string|]

    [$p|(a: String) is-prefix-of?: (b: String)|] =: do
        a <- getText [$e|a|]
        b <- getText [$e|b|]
        bool (T.isPrefixOf a b)

    [$p|(a: String) is-suffix-of?: (b: String)|] =: do
        a <- getText [$e|a|]
        b <- getText [$e|b|]
        bool (T.isSuffixOf a b)

    [$p|(a: String) is-infix-of?: (b: String)|] =: do
        a <- getText [$e|a|]
        b <- getText [$e|b|]
        bool (T.isInfixOf a b)

    [$p|(a: String) starts-with?: (b: String)|] =::: [$e|b is-prefix-of?: a|]
    [$p|(a: String) ends-with?: (b: String)|] =::: [$e|b is-suffix-of?: a|]
    [$p|(a: String) includes?: (b: String)|] =::: [$e|b is-infix-of?: a|]

    [$p|(s: String) filter: b|] =::: [$e|s (as: List) (filter: b) to-string|]

    [$p|(x: String) zip: (y: String)|] =::: [$e|x zip: y with: @->|]
    [$p|(x: String) zip: (y: String) with: z|] =: do
        x <- getText [$e|x|]
        y <- getText [$e|y|]
        z <- here "z"

        vs <- forM (T.zip x y) $ \(a, b) -> do
            as <- list [Char a, Char b]
            dispatch (keyword ["call"] [z, as])

        list vs
