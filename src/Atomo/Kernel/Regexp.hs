{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Regexp where

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Regex.PCRE
import qualified Data.ByteString as BS

import Atomo

byteString :: BS.ByteString -> Value
byteString = String . decodeUtf8

load :: VM ()
load = do
    ([$p|RegexpMatch|] =::) =<< eval [$e|Object clone|]

    [$p|(r: Regexp) matches?: (s: String)|] =: do
        Regexp r _ _ <- here "r" >>= findRegexp
        t <- getText [$e|s|]
        return (Boolean (match r (encodeUtf8 t)))

    [$p|(r: Regexp) match: (s: String)|] =: do
        Regexp r _ _ <- here "r" >>= findRegexp
        t <- getText [$e|s|]
        let mr = match r (encodeUtf8 t) :: MatchResult BS.ByteString
        if BS.null (mrMatch mr)
            then return (particle "none")
            else do
                rm <- [$e|RegexpMatch|] `newWith`
                    [ ("before", byteString (mrBefore mr))
                    , ("match", byteString (mrMatch mr))
                    , ("after", byteString (mrAfter mr))
                    , ("captures", list (map byteString (mrSubList mr)))
                    ]

                return (keyParticleN ["ok"] [rm])

    [$p|(r: Regexp) =~ (s: String)|] =::: [$e|r matches?: s|]
    [$p|(s: String) =~ (r: Regexp)|] =::: [$e|r matches?: s|]
