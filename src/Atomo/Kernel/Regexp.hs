{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Regexp where

import Control.Arrow
import Data.Char (isDigit)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Regex.PCRE
import qualified Data.Array as A
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Atomo

byteString :: BS.ByteString -> Value
byteString = String . decodeUtf8

data MkRegex a = RegexOK a | Failed String

instance Monad MkRegex where
    return = RegexOK
    fail = Failed

    Failed x >>= _ = Failed x
    RegexOK a >>= f = f a

data REReplace
    = REChunk T.Text
    | REReference Int
    | RENamed String

load :: VM ()
load = do
    ([$p|RegexpBindings|] =::) =<< eval [$e|Object clone|]
    ([$p|RegexpMatch|] =::) =<< eval [$e|Object clone|]

    [$p|Regexp new: (s: String) &flags: ""|] =: do
        s <- getString [$e|s|]
        fs <- getString [$e|flags|]

        case regex s fs of
            RegexOK re ->
                return (Regexp re s fs (namedCaptures s))
            Failed x ->
                raise ["regexp-failed"] [string x]

    [$p|(r: Regexp) matches?: (s: String)|] =: do
        Regexp { rCompiled = re } <- here "r" >>= findRegexp
        t <- getText [$e|s|]
        return (Boolean (match re (encodeUtf8 t)))

    [$p|(r: Regexp) match: (s: String)|] =: do
        Regexp { rCompiled = re, rNamed = ns } <- here "r" >>= findRegexp
        t <- getText [$e|s|]
        let mr = match re (encodeUtf8 t) :: MatchResult BS.ByteString
        if BS.null (mrMatch mr)
            then return (particle "none")
            else do
                bs <- mkBindings (mrMatch mr:mrSubList mr) ns

                rm <- [$e|RegexpMatch|] `newWith`
                    [ ("before", byteString (mrBefore mr))
                    , ("match", byteString (mrMatch mr))
                    , ("after", byteString (mrAfter mr))
                    , ("captures", list (map byteString (mrSubList mr)))
                    , ("bindings", bs)
                    ]

                return (keyParticleN ["ok"] [rm])

    [$p|(s: String) replace: (r: Regexp) with: (callback: Block)|] =: do
        Regexp { rCompiled = re, rNamed = ns } <- here "r" >>= findRegexp
        callback <- here "callback"
        t <- getText [$e|s|]
        doReplace re t $ \cs -> do
            bs <- mkBindings cs ns
            dispatch (keyword ["join"] [bs, callback])
                >>= liftM fromString . findString

    [$p|(s: String) replace: (r: Regexp) with: (format: String)|] =: do
        Regexp { rCompiled = re, rNamed = ns } <- here "r" >>= findRegexp
        format <- getText [$e|format|]
        t <- getText [$e|s|]
        doReplace re t (reReplace (replacements format) ns)

    [$p|(s: String) replace-all: (r: Regexp) with: (callback: Block)|] =: do
        Regexp { rCompiled = re, rNamed = ns } <- here "r" >>= findRegexp
        callback <- here "callback"
        t <- getText [$e|s|]
        doReplaceAll re t $ \cs -> do
            bs <- mkBindings cs ns
            dispatch (keyword ["join"] [bs, callback])
                >>= liftM fromString . findString

    [$p|(s: String) replace-all: (r: Regexp) with: (format: String)|] =: do
        Regexp { rCompiled = re, rNamed = ns } <- here "r" >>= findRegexp
        format <- getText [$e|format|]
        t <- getText [$e|s|]
        doReplaceAll re t (reReplace (replacements format) ns)

    [$p|(r: Regexp) =~ (s: String)|] =::: [$e|r matches?: s|]
    [$p|(s: String) =~ (r: Regexp)|] =::: [$e|r matches?: s|]

doReplace :: Regex -> T.Text -> ([BS.ByteString] -> VM T.Text) -> VM Value
doReplace re t f =
    case ms of
        [] -> return (String t)
        ((s, (o, l)):cs) -> do
            r <- f (s:map fst cs)
            return . String . T.concat $
                [ T.take o t
                , r
                , T.drop (o + l) t
                ]
  where
    ms = getAllTextSubmatches (match re (encodeUtf8 t))

doReplaceAll :: Regex -> T.Text -> ([BS.ByteString] -> VM T.Text) -> VM Value
doReplaceAll re t f = do
    rs <- forM (map (map fst) ms) f
    return (String (fuse cs rs))
  where
    marr :: AllTextMatches (A.Array Int) (MatchText BS.ByteString)
    marr = match re (encodeUtf8 t)

    ms :: [[(BS.ByteString, (MatchOffset, MatchLength))]]
    ms = map A.elems . A.elems $ getAllTextMatches marr

    cs = chunks t (map (snd . head) ms)

chunks :: T.Text -> [(MatchOffset, MatchLength)] -> [T.Text]
chunks t [] = [t]
chunks t ((o, l):xs) = c : chunks rest updated
  where
    c = T.take o t
    rest = T.drop (o + l) t
    updated = map (first (flip (-) (o + l))) xs

fuse :: [T.Text] -> [T.Text] -> T.Text
fuse (x:xs) (r:rs) = T.concat [x, r, fuse xs rs]
fuse xs ys = T.concat (xs ++ ys)

replacements :: T.Text -> [REReplace]
replacements t
    | T.null t = []
    | T.head t == '$' && t `T.index` 1 == '(' =
        RENamed name : replacements afterName
    | T.head t == '$' =
        REReference (read num) : replacements afterNum
    | otherwise =
        REChunk chunk : replacements rest
  where
    name = T.unpack (T.takeWhile (/= ')') (T.drop 2 t))
    afterName = T.tail (T.dropWhile (/= ')') (T.drop 2 t))

    num = T.unpack (T.takeWhile isDigit (T.tail t))
    afterNum = T.dropWhile isDigit (T.tail t)

    (chunk, rest) = T.span (/= '$') t

reReplace :: [REReplace] -> [(String, Int)] -> [BS.ByteString] -> VM T.Text
reReplace [] _ _ = return T.empty
reReplace (REChunk c:rs) ns bs = liftM (c `T.append`) (reReplace rs ns bs)
reReplace (REReference n:rs) ns bs =
    liftM (decodeUtf8 (bs !! n) `T.append`) (reReplace rs ns bs)
reReplace (RENamed n:rs) ns bs =
    case lookup n ns of
        Nothing -> raise ["unknown-regexp-reference"] [string n]
        Just i -> do
            rest <- reReplace rs ns bs
            return (decodeUtf8 (bs !! i) `T.append` rest)

mkBindings :: [BS.ByteString] -> [(String, Int)] -> VM Value
mkBindings subs names =
    [$e|RegexpBindings|] `newWith` concat
        [ zipWith (\n m -> ("\\" ++ show n, byteString m))
            [0 :: Int ..]
            subs
        , map (\(n, o) -> ("\\" ++ n, byteString (subs !! o))) names
        ]