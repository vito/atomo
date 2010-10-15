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
        Integer n <- here "n" >>= findInteger
        liftIO (sleepFor n)
        return (particle "ok")

    [$p|Timer sleep: (d: Double)|] =: do
        Double d <- here "d" >>= findDouble
        liftIO (threadDelay (floor d))
        return (particle "ok")

    prelude


sleepFor :: Integer -> IO ()
sleepFor n
    | n > fromIntegral limit = threadDelay limit >> sleepFor (n - fromIntegral limit)
    | otherwise = threadDelay (fromIntegral n)
  where
    limit = maxBound :: Int

prelude :: VM ()
prelude = mapM_ eval [$es|
    Timer do: (b: Block) every: (n: Number) :=
      { { Timer sleep: n; b spawn } repeat } spawn

    Timer do: (b: Block) after: (n: Number) :=
      { Timer sleep: n; b call } spawn

    (b: Block) time :=
      { before = Timer now
        b call
        after = Timer now
        after - before
      } call

    -- units!
    (n: Number) us := n
    (n: Number) microseconds := n us
    (n: Number) microsecond := n us

    (n: Number) ms := n * 1000
    (n: Number) milliseconds := n ms
    (n: Number) millisecond := n ms

    (n: Number) seconds := n * 1000000
    (n: Number) second := n seconds

    (n: Number) minutes := (n * 60) seconds
    (n: Number) minute := n minutes

    (n: Number) hours := (n * 60) minutes
    (n: Number) hour := n hours

    (n: Number) days := (n * 24) hours
    (n: Number) day := n days

    (n: Number) weeks := (n * 7) days
    (n: Number) week := n weeks

    (n: Number) months := (n * 30) days
    (n: Number) month := n months

    (n: Number) years := (n * 365) days
    (n: Number) year := n years
|]
