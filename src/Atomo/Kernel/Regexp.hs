{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Regexp where

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Regex.PCRE
import qualified Data.ByteString as BS

import Atomo

byteString :: BS.ByteString -> Value
byteString = String . decodeUtf8

data MkRegex a = RegexOK a | Failed String

instance Monad MkRegex where
    return = RegexOK
    fail = Failed

    Failed x >>= _ = Failed x
    RegexOK a >>= f = f a

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
        Regexp { rCompiled = r } <- here "r" >>= findRegexp
        t <- getText [$e|s|]
        return (Boolean (match r (encodeUtf8 t)))

    [$p|(r: Regexp) match: (s: String)|] =: do
        Regexp { rCompiled = r, rNamed = ns } <- here "r" >>= findRegexp
        t <- getText [$e|s|]
        let mr = match r (encodeUtf8 t) :: MatchResult BS.ByteString
        if BS.null (mrMatch mr)
            then return (particle "none")
            else do
                bs <- [$e|RegexpBindings|] `newWith` concat
                    [ [("\\0", byteString (mrMatch mr))]
                    , zipWith (\n m -> ("\\" ++ show n, byteString m)) [1 :: Int ..] (mrSubList mr)
                    , map (\(n, o) -> ("\\" ++ n, byteString (mrSubList mr !! o))) ns
                    ]

                rm <- [$e|RegexpMatch|] `newWith`
                    [ ("before", byteString (mrBefore mr))
                    , ("match", byteString (mrMatch mr))
                    , ("after", byteString (mrAfter mr))
                    , ("captures", list (map byteString (mrSubList mr)))
                    , ("bindings", bs)
                    ]

                return (keyParticleN ["ok"] [rm])

    [$p|(r: Regexp) =~ (s: String)|] =::: [$e|r matches?: s|]
    [$p|(s: String) =~ (r: Regexp)|] =::: [$e|r matches?: s|]
