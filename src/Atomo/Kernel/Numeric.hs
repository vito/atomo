{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Numeric (load) where

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    eval [$e|Object clone|] >>= ([$p|Number|] =::)

    eval [$e|Integer delegates-to: Number|]
    eval [$e|Double delegates-to: Number|]

    [$p|(a: Integer) sqrt|] =: do
        Integer a <- here "a" >>= findValue isInteger
        return (Double (sqrt (fromIntegral a)))

    [$p|(a: Double) sqrt|] =: do
        Double a <- here "a" >>= findValue isDouble
        return (Double (sqrt a))

    [$p|(a: Integer) + (b: Integer)|] =: primII (+)
    [$p|(a: Integer) + (b: Double)|] =: primID (+)
    [$p|(a: Double) + (b: Integer)|] =: primDI (+)
    [$p|(a: Double) + (b: Double)|] =: primDD (+)
    [$p|(a: Integer) - (b: Integer)|] =: primII (-)
    [$p|(a: Integer) - (b: Double)|] =: primID (-)
    [$p|(a: Double) - (b: Integer)|] =: primDI (-)
    [$p|(a: Double) - (b: Double)|] =: primDD (-)
    [$p|(a: Integer) * (b: Integer)|] =: primII (*)
    [$p|(a: Integer) * (b: Double)|] =: primID (*)
    [$p|(a: Double) * (b: Integer)|] =: primDI (*)
    [$p|(a: Double) * (b: Double)|] =: primDD (*)
    [$p|(a: Integer) / (b: Integer)|] =: primII div
    [$p|(a: Integer) / (b: Double)|] =: primID (/)
    [$p|(a: Double) / (b: Integer)|] =: primDI (/)
    [$p|(a: Double) / (b: Double)|] =: primDD (/)
    [$p|(a: Integer) ^ (b: Integer)|] =: primII (^)
    [$p|(a: Integer) ^ (b: Double)|] =: primID (**)
    [$p|(a: Double) ^ (b: Integer)|] =: primDI (**)
    [$p|(a: Double) ^ (b: Double)|] =: primDD (**)

    [$p|(a: Integer) % (b: Integer)|] =: primII mod
    [$p|(a: Integer) quotient: (b: Integer)|] =: primII quot
    [$p|(a: Integer) remainder: (b: Integer)|] =: primII rem
  where
    primII f = do
        Integer a <- here "a" >>= findValue isInteger
        Integer b <- here "b" >>= findValue isInteger
        return (Integer (f a b))

    primID f = do
        Integer a <- here "a" >>= findValue isInteger
        Double b <- here "b" >>= findValue isDouble
        return (Double (f (fromIntegral a) b))

    primDI f = do
        Double a <- here "a" >>= findValue isDouble
        Integer b <- here "b" >>= findValue isInteger
        return (Double (f a (fromIntegral b)))

    primDD f = do
        Double a <- here "a" >>= findValue isDouble
        Double b <- here "b" >>= findValue isDouble
        return (Double (f a b))
