{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Numeric (load) where

import Data.Ratio

import Atomo


load :: VM ()
load = do
    mapM_ eval
        [ [$e|operator right 8 ^|]
        , [$e|operator 7 % * /|]
        , [$e|operator 6 + -|]
        ]

    eval [$e|Object clone|] >>= ([$p|Number|] =::)

    eval [$e|Integer delegates-to: Number|]
    eval [$e|Double delegates-to: Number|]

    [$p|(i: Integer) sqrt|] =: do
        Integer i <- here "i" >>= findInteger
        return (Double (sqrt (fromIntegral i)))

    [$p|(d: Double) sqrt|] =: do
        Double d <- here "d" >>= findDouble
        return (Double (sqrt d))

    [$p|(d: Double) ceiling|] =: do
        Double d <- here "d" >>= findDouble
        return (Integer (ceiling d))

    [$p|(d: Double) round|] =: do
        Double d <- here "d" >>= findDouble
        return (Integer (round d))

    [$p|(d: Double) floor|] =: do
        Double d <- here "d" >>= findDouble
        return (Integer (floor d))

    [$p|(i: Integer) reciprocal|] =: do
        Integer i <- here "i" >>= findInteger
        return (Double (recip (fromIntegral i)))

    [$p|(d: Double) reciprocal|] =: do
        Double d <- here "d" >>= findDouble
        return (Double (recip d))

    [$p|(r: Rational) reciprocal|] =: do
        Rational r <- here "r" >>= findRational
        return (Rational (recip r))

    [$p|(r: Rational) numerator|] =: do
        Rational r <- here "r" >>= findRational
        return (Integer (numerator r))

    [$p|(r: Rational) denominator|] =: do
        Rational r <- here "r" >>= findRational
        return (Integer (denominator r))

    [$p|(d: Double) as: Integer|] =::: [$e|d floor|]
    [$p|(d: Double) as: Rational|] =::: [$e|d rationalize|]
    [$p|(i: Integer) as: Double|] =: do
        Integer i <- here "i" >>= findInteger
        return (Double (fromIntegral i))
    [$p|(i: Integer) as: Rational|] =: do
        Integer i <- here "i" >>= findInteger
        return (Rational (i % 1))
    [$p|(r: Rational) as: Double|] =::: [$e|r approximate|]
    [$p|(r: Rational) as: Integer|] =::: [$e|r approximate floor|]

    [$p|(i: Integer) rationalize|] =::: [$e|(i as: Double) rationalize|]
    [$p|(d: Double) rationalize|] =::: [$e|d rationalize: 0.001|]
    [$p|(d: Double) rationalize: (e: Double)|] =: do
        Double d <- here "d" >>= findDouble
        Double e' <- here "e" >>= findDouble
        return (Rational (approxRational d e'))

    [$p|(r: Rational) approximate|] =: do
        Rational r <- here "r" >>= findRational
        return (Double (fromRational r))

    [$p|(a: Integer) + (b: Integer)|] =: primII (+)
    [$p|(a: Rational) + (b: Rational)|] =: primRR (+)
    [$p|(a: Double) + (b: Double)|] =: primDD (+)
    [$p|(a: Integer) + (b: Double)|] =: primID (+)
    [$p|(a: Integer) + (b: Rational)|] =: primIR (+)
    [$p|(a: Double) + (b: Integer)|] =: primDI (+)
    [$p|(a: Double) + (b: Rational)|] =: primDR (+)
    [$p|(a: Rational) + (b: Integer)|] =: primRI (+)
    [$p|(a: Rational) + (b: Double)|] =: primRD (+)

    [$p|(a: Integer) - (b: Integer)|] =: primII (-)
    [$p|(a: Rational) - (b: Rational)|] =: primRR (-)
    [$p|(a: Double) - (b: Double)|] =: primDD (-)
    [$p|(a: Integer) - (b: Double)|] =: primID (-)
    [$p|(a: Integer) - (b: Rational)|] =: primIR (-)
    [$p|(a: Double) - (b: Integer)|] =: primDI (-)
    [$p|(a: Double) - (b: Rational)|] =: primDR (-)
    [$p|(a: Rational) - (b: Integer)|] =: primRI (-)
    [$p|(a: Rational) - (b: Double)|] =: primRD (-)

    [$p|(a: Integer) * (b: Integer)|] =: primII (*)
    [$p|(a: Rational) * (b: Rational)|] =: primRR (*)
    [$p|(a: Double) * (b: Double)|] =: primDD (*)
    [$p|(a: Integer) * (b: Double)|] =: primID (*)
    [$p|(a: Integer) * (b: Rational)|] =: primIR (*)
    [$p|(a: Double) * (b: Integer)|] =: primDI (*)
    [$p|(a: Double) * (b: Rational)|] =: primDR (*)
    [$p|(a: Rational) * (b: Integer)|] =: primRI (*)
    [$p|(a: Rational) * (b: Double)|] =: primRD (*)

    [$p|(a: Integer) / (b: Integer)|] =: primII div
    [$p|(a: Rational) / (b: Rational)|] =: primRR (/)
    [$p|(a: Double) / (b: Double)|] =: primDD (/)
    [$p|(a: Integer) / (b: Double)|] =: primID (/)
    [$p|(a: Integer) / (b: Rational)|] =: primIR (/)
    [$p|(a: Double) / (b: Integer)|] =: primDI (/)
    [$p|(a: Double) / (b: Rational)|] =: primDR (/)
    [$p|(a: Rational) / (b: Integer)|] =: primRI (/)
    [$p|(a: Rational) / (b: Double)|] =: primRD (/)

    [$p|(a: Integer) ^ (b: Integer)|] =: primII (^)
    [$p|(a: Double) ^ (b: Double)|] =: primDD (**)
    [$p|(a: Integer) ^ (b: Double)|] =: primID (**)
    [$p|(a: Double) ^ (b: Integer)|] =: primDI (**)
    [$p|(a: Rational) ^ (b: Integer)|] =: do
        Rational a <- here "a" >>= findRational
        Integer b <- here "b" >>= findInteger
        return (Rational (a ^ b))

    [$p|(a: Integer) % (b: Integer)|] =: primII mod
    [$p|(a: Integer) quotient: (b: Integer)|] =: primII quot
    [$p|(a: Integer) remainder: (b: Integer)|] =: primII rem
  where
    primII f = do
        Integer a <- here "a" >>= findInteger
        Integer b <- here "b" >>= findInteger
        return (Integer (f a b))

    primDD f = do
        Double a <- here "a" >>= findDouble
        Double b <- here "b" >>= findDouble
        return (Double (f a b))

    primRR f = do
        Rational a <- here "a" >>= findRational
        Rational b <- here "b" >>= findRational
        return (Rational (f a b))

    primID f = do
        Integer a <- here "a" >>= findInteger
        Double b <- here "b" >>= findDouble
        return (Double (f (fromIntegral a) b))

    primIR f = do
        Integer a <- here "a" >>= findInteger
        Rational b <- here "b" >>= findRational
        return (Rational (f (toRational a) b))

    primDI f = do
        Double a <- here "a" >>= findDouble
        Integer b <- here "b" >>= findInteger
        return (Double (f a (fromIntegral b)))

    primDR f = do
        Double a <- here "a" >>= findDouble
        Rational b <- here "b" >>= findRational
        return (Rational (f (toRational a) b))

    primRD f = do
        Rational a <- here "a" >>= findRational
        Double b <- here "b" >>= findDouble
        return (Rational (f a (toRational b)))

    primRI f = do
        Rational a <- here "a" >>= findRational
        Integer b <- here "b" >>= findInteger
        return (Rational (f a (toRational b)))
