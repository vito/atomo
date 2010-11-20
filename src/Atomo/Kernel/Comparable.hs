{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Comparable (load) where

import qualified Data.Vector as V

import Atomo


load :: VM ()
load = do
    mapM_ eval
        [ [$e|operator 4 == /= < <= >= >|]
        , [$e|operator right 3 &&|]
        , [$e|operator right 2 |||]
        ]

    [$p|a equals?: b|] =: do
        a <- here "a"
        b <- here "b"
        return $ Boolean (a == b)

    [$p|(a: Object) == (b: Object)|] =::: [$e|a equals?: b|]
    [$p|(a: Object) /= (b: Object)|] =::: [$e|(a == b) not|]

    [$p|(a: Char) < (b: Char)|] =: do
        Char a <- here "a" >>= findChar
        Char b <- here "b" >>= findChar
        return $ Boolean (a < b)

    [$p|(a: Char) > (b: Char)|] =: do
        Char a <- here "a" >>= findChar
        Char b <- here "b" >>= findChar
        return $ Boolean (a > b)

    [$p|(a: Char) <= (b: Char)|] =: do
        Char a <- here "a" >>= findChar
        Char b <- here "b" >>= findChar
        return $ Boolean (a <= b)

    [$p|(a: Char) >= (b: Char)|] =: do
        Char a <- here "a" >>= findChar
        Char b <- here "b" >>= findChar
        return $ Boolean (a >= b)

    [$p|(a: Char) == (b: Char)|] =: do
        Char a <- here "a" >>= findChar
        Char b <- here "b" >>= findChar
        return $ Boolean (a == b)

    [$p|(a: Integer) < (b: Integer)|] =: primII (<)
    [$p|(a: Double) < (b: Double)|] =: primDD (<)
    [$p|(a: Rational) < (b: Rational)|] =: primRR (<)
    [$p|(a: Integer) < (b: Double)|] =: primID (<)
    [$p|(a: Integer) < (b: Rational)|] =: primIR (<)
    [$p|(a: Double) < (b: Integer)|] =: primDI (<)
    [$p|(a: Double) < (b: Rational)|] =: primDR (<)
    [$p|(a: Rational) < (b: Integer)|] =: primRI (<)
    [$p|(a: Rational) < (b: Double)|] =: primRD (<)

    [$p|(a: Integer) > (b: Integer)|] =: primII (>)
    [$p|(a: Double) > (b: Double)|] =: primDD (>)
    [$p|(a: Rational) > (b: Rational)|] =: primRR (>)
    [$p|(a: Integer) > (b: Double)|] =: primID (>)
    [$p|(a: Integer) > (b: Rational)|] =: primIR (>)
    [$p|(a: Double) > (b: Integer)|] =: primDI (>)
    [$p|(a: Double) > (b: Rational)|] =: primDR (>)
    [$p|(a: Rational) > (b: Integer)|] =: primRI (>)
    [$p|(a: Rational) > (b: Double)|] =: primRD (>)

    [$p|(a: Integer) <= (b: Integer)|] =: primII (<=)
    [$p|(a: Double) <= (b: Double)|] =: primDD (<=)
    [$p|(a: Rational) <= (b: Rational)|] =: primRR (<=)
    [$p|(a: Integer) <= (b: Double)|] =: primID (<=)
    [$p|(a: Integer) <= (b: Rational)|] =: primIR (<=)
    [$p|(a: Double) <= (b: Integer)|] =: primDI (<=)
    [$p|(a: Double) <= (b: Rational)|] =: primDR (<=)
    [$p|(a: Rational) <= (b: Integer)|] =: primRI (<=)
    [$p|(a: Rational) <= (b: Double)|] =: primRD (<=)

    [$p|(a: Integer) >= (b: Integer)|] =: primII (>=)
    [$p|(a: Double) >= (b: Double)|] =: primDD (>=)
    [$p|(a: Rational) >= (b: Rational)|] =: primRR (>=)
    [$p|(a: Integer) >= (b: Double)|] =: primID (>=)
    [$p|(a: Integer) >= (b: Rational)|] =: primIR (>=)
    [$p|(a: Double) >= (b: Integer)|] =: primDI (>=)
    [$p|(a: Double) >= (b: Rational)|] =: primDR (>=)
    [$p|(a: Rational) >= (b: Integer)|] =: primRI (>=)
    [$p|(a: Rational) >= (b: Double)|] =: primRD (>=)

    [$p|(a: Integer) == (b: Integer)|] =: primII (==)
    [$p|(a: Double) == (b: Double)|] =: primDD (==)
    [$p|(a: Rational) == (b: Rational)|] =: primRR (==)
    [$p|(a: Integer) == (b: Double)|] =: primID (==)
    [$p|(a: Integer) == (b: Rational)|] =: primIR (==)
    [$p|(a: Double) == (b: Integer)|] =: primDI (==)
    [$p|(a: Double) == (b: Rational)|] =: primDR (==)
    [$p|(a: Rational) == (b: Integer)|] =: primRI (==)
    [$p|(a: Rational) == (b: Double)|] =: primRD (==)

    [$p|(a: List) == (b: List)|] =: do
        as <- getVector [$e|a|]
        bs <- getVector [$e|b|]

        if V.length as == V.length bs
            then do
                eqs <- V.zipWithM (\a b -> dispatch (keyword ["=="] [a, b])) as bs
                return $ Boolean (V.all (== Boolean True) eqs)
            else return $ Boolean False

    [$p|(a: Process) == (b: Process)|] =: do
        Process _ a <- here "a" >>= findProcess
        Process _ b <- here "b" >>= findProcess
        return $ Boolean (a == b)

    [$p|(a: Message) == (b: Message)|] =: do
        Message a <- here "a" >>= findMessage
        Message b <- here "b" >>= findMessage

        case (a, b) of
            (Single ai _ at, Single bi _ bt) -> do
                Boolean t <- dispatch (keyword ["=="] [at, bt]) >>= findBoolean
                return $ Boolean (ai == bi && t)
            (Keyword ai _ avs, Keyword bi _ bvs)
                | ai == bi && length avs == length bvs -> do
                eqs <- zipWithM (\x y -> dispatch (keyword ["=="] [x, y])) avs bvs
                return $ Boolean (all (== Boolean True) eqs)
            _ -> return $ Boolean False

    [$p|(a: Particle) == (b: Particle)|] =: do
        Particle a <- here "a" >>= findParticle
        Particle b <- here "b" >>= findParticle

        case (a, b) of
            (PMSingle an, PMSingle bn) ->
                return $ Boolean (an == bn)
            (PMKeyword ans avs, PMKeyword bns bvs)
                | ans == bns && length avs == length bvs -> do
                eqs <- zipWithM (\mx my ->
                    case (mx, my) of
                        (Nothing, Nothing) -> return (Boolean True)
                        (Just x, Just y) ->
                            dispatch (keyword ["=="] [x, y])
                        _ -> return $ Boolean False) avs bvs
                return $ Boolean (all (== Boolean True) eqs)
            _ -> return $ Boolean False
  where
    primII f = do
        Integer a <- here "a" >>= findInteger
        Integer b <- here "b" >>= findInteger
        return (Boolean $ f a b)

    primDD f = do
        Double a <- here "a" >>= findDouble
        Double b <- here "b" >>= findDouble
        return (Boolean $ f a b)

    primRR f = do
        Rational a <- here "a" >>= findRational
        Rational b <- here "b" >>= findRational
        return (Boolean $ f a b)

    primID f = do
        Integer a <- here "a" >>= findInteger
        Double b <- here "b" >>= findDouble
        return (Boolean $ f (fromIntegral a) b)

    primIR f = do
        Integer a <- here "a" >>= findInteger
        Rational b <- here "b" >>= findRational
        return (Boolean $ f (toRational a) b)

    primDI f = do
        Double a <- here "a" >>= findDouble
        Integer b <- here "b" >>= findInteger
        return (Boolean $ f a (fromIntegral b))

    primDR f = do
        Double a <- here "a" >>= findDouble
        Rational b <- here "b" >>= findRational
        return (Boolean $ f (toRational a) b)

    primRD f = do
        Rational a <- here "a" >>= findRational
        Double b <- here "b" >>= findDouble
        return (Boolean $ f a (toRational b))

    primRI f = do
        Rational a <- here "a" >>= findRational
        Integer b <- here "b" >>= findInteger
        return (Boolean $ f a (toRational b))
