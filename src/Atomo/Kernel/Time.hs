{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Time (load) where

import Data.Time.Clock.POSIX (getPOSIXTime)

import Atomo


load :: VM ()
load = do
    ([$p|Timer|] =::) =<< eval [$e|Object clone|]

    [$p|Timer now|] =:
        liftM (Double . fromRational . toRational) (liftIO getPOSIXTime)

    [$p|Timer sleep: (n: Integer)|] =: do
        Integer n <- here "n" >>= findInteger
        liftIO (sleepFor n)
        return (particle "ok")

    [$p|Timer sleep: (d: Double)|] =: do
        Double d <- here "d" >>= findDouble
        liftIO (threadDelay (floor d))
        return (particle "ok")


sleepFor :: Integer -> IO ()
sleepFor n
    | n > fromIntegral limit = threadDelay limit >> sleepFor (n - fromIntegral limit)
    | otherwise = threadDelay (fromIntegral n)
  where
    limit = maxBound :: Int
