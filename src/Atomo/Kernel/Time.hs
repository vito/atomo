{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Time (load) where

import Data.Time.Clock.POSIX (getPOSIXTime)

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    ([$p|Timer|] =::) =<< eval [$e|Object clone|]

    [$p|Timer now|] =:
        fmap (Double . fromRational . toRational) (liftIO getPOSIXTime)

    [$p|Timer sleep: (n: Integer)|] =: do
        Integer n <- here "n" >>= findValue isInteger
        liftIO (threadDelay (fromIntegral n))
        return (particle "ok")

    [$p|Timer sleep: (d: Double)|] =: do
        Double d <- here "d" >>= findValue isDouble
        liftIO (threadDelay (floor d))
        return (particle "ok")
