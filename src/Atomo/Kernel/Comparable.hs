{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Comparable (load) where

import qualified Data.Vector as V

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    mapM_ eval $
        [ [$e|operator 4 ==, /=, <, <=, >=, >|]
        , [$e|operator right 3 &&|]
        , [$e|operator right 2 |||]
        ]

    [$p|a equals?: b|] =: do
        a <- here "a"
        b <- here "b"
        bool (a == b)

    [$p|(a: Object) == (b: Object)|] =::: [$e|a equals?: b|]
    [$p|(a: Object) /= (b: Object)|] =::: [$e|(a == b) not|]

    [$p|(a: Char) < (b: Char)|] =: do
        Char a <- here "a" >>= findChar
        Char b <- here "b" >>= findChar
        bool (a < b)

    [$p|(a: Integer) < (b: Integer)|] =: do
        Integer a <- here "a" >>= findInteger
        Integer b <- here "b" >>= findInteger
        bool (a < b)

    [$p|(a: Integer) < (b: Double)|] =: do
        Integer a <- here "a" >>= findInteger
        Double b <- here "b" >>= findDouble
        bool (fromIntegral a < b)

    [$p|(a: Double) < (b: Integer)|] =: do
        Double a <- here "a" >>= findDouble
        Integer b <- here "b" >>= findInteger
        bool (a < fromIntegral b)

    [$p|(a: Double) < (b: Double)|] =: do
        Double a <- here "a" >>= findDouble
        Double b <- here "b" >>= findDouble
        bool (a < b)

    [$p|(a: Char) > (b: Char)|] =: do
        Char a <- here "a" >>= findChar
        Char b <- here "b" >>= findChar
        bool (a > b)

    [$p|(a: Integer) > (b: Integer)|] =: do
        Integer a <- here "a" >>= findInteger
        Integer b <- here "b" >>= findInteger
        bool (a > b)

    [$p|(a: Integer) > (b: Double)|] =: do
        Integer a <- here "a" >>= findInteger
        Double b <- here "b" >>= findDouble
        bool (fromIntegral a > b)

    [$p|(a: Double) > (b: Integer)|] =: do
        Double a <- here "a" >>= findDouble
        Integer b <- here "b" >>= findInteger
        bool (a > fromIntegral b)

    [$p|(a: Double) > (b: Double)|] =: do
        Double a <- here "a" >>= findDouble
        Double b <- here "b" >>= findDouble
        bool (a > b)

    [$p|(a: Char) <= (b: Char)|] =: do
        Char a <- here "a" >>= findChar
        Char b <- here "b" >>= findChar
        bool (a <= b)

    [$p|(a: Integer) <= (b: Integer)|] =: do
        Integer a <- here "a" >>= findInteger
        Integer b <- here "b" >>= findInteger
        bool (a <= b)

    [$p|(a: Integer) <= (b: Double)|] =: do
        Integer a <- here "a" >>= findInteger
        Double b <- here "b" >>= findDouble
        bool (fromIntegral a <= b)

    [$p|(a: Double) <= (b: Integer)|] =: do
        Double a <- here "a" >>= findDouble
        Integer b <- here "b" >>= findInteger
        bool (a <= fromIntegral b)

    [$p|(a: Double) <= (b: Double)|] =: do
        Double a <- here "a" >>= findDouble
        Double b <- here "b" >>= findDouble
        bool (a <= b)

    [$p|(a: Char) >= (b: Char)|] =: do
        Char a <- here "a" >>= findChar
        Char b <- here "b" >>= findChar
        bool (a >= b)

    [$p|(a: Integer) >= (b: Integer)|] =: do
        Integer a <- here "a" >>= findInteger
        Integer b <- here "b" >>= findInteger
        bool (a >= b)

    [$p|(a: Integer) >= (b: Double)|] =: do
        Integer a <- here "a" >>= findInteger
        Double b <- here "b" >>= findDouble
        bool (fromIntegral a >= b)

    [$p|(a: Double) >= (b: Integer)|] =: do
        Double a <- here "a" >>= findDouble
        Integer b <- here "b" >>= findInteger
        bool (a >= fromIntegral b)

    [$p|(a: Double) >= (b: Double)|] =: do
        Double a <- here "a" >>= findDouble
        Double b <- here "b" >>= findDouble
        bool (a >= b)

    [$p|(a: Char) == (b: Char)|] =: do
        Char a <- here "a" >>= findChar
        Char b <- here "b" >>= findChar
        bool (a == b)

    [$p|(a: Integer) == (b: Integer)|] =: do
        Integer a <- here "a" >>= findInteger
        Integer b <- here "b" >>= findInteger
        bool (a == b)

    [$p|(a: Integer) == (b: Double)|] =: do
        Integer a <- here "a" >>= findInteger
        Double b <- here "b" >>= findDouble
        bool (fromIntegral a == b)

    [$p|(a: Double) == (b: Integer)|] =: do
        Double a <- here "a" >>= findDouble
        Integer b <- here "b" >>= findInteger
        bool (a == fromIntegral b)

    [$p|(a: Double) == (b: Double)|] =: do
        Double a <- here "a" >>= findDouble
        Double b <- here "b" >>= findDouble
        bool (a == b)

    [$p|(a: List) == (b: List)|] =: do
        as <- getVector [$e|a|]
        bs <- getVector [$e|b|]

        if V.length as == V.length bs
            then do
                eqs <- V.zipWithM (\a b -> dispatch (keyword ["=="] [a, b])) as bs
                true <- bool True
                bool (V.all (== true) eqs)
            else bool False

    [$p|(a: Process) == (b: Process)|] =: do
        Process _ a <- here "a" >>= findProcess
        Process _ b <- here "b" >>= findProcess
        bool (a == b)

    [$p|(a: Message) == (b: Message)|] =: do
        Message a <- here "a" >>= findMessage
        Message b <- here "b" >>= findMessage

        true <- bool True
        case (a, b) of
            (Single ai _ at, Single bi _ bt) -> do
                t <- dispatch (keyword ["=="] [at, bt])
                bool (ai == bi && t == true)
            (Keyword ai _ avs, Keyword bi _ bvs)
                | ai == bi && length avs == length bvs -> do
                eqs <- zipWithM (\x y -> dispatch (keyword ["=="] [x, y])) avs bvs
                bool (all (== true) eqs)
            _ -> bool False

    [$p|(a: Particle) == (b: Particle)|] =: do
        Particle a <- here "a" >>= findParticle
        Particle b <- here "b" >>= findParticle

        true <- bool True
        case (a, b) of
            (PMSingle an, PMSingle bn) ->
                bool (an == bn)
            (PMKeyword ans avs, PMKeyword bns bvs)
                | ans == bns && length avs == length bvs -> do
                eqs <- zipWithM (\mx my ->
                    case (mx, my) of
                        (Nothing, Nothing) -> return true
                        (Just x, Just y) ->
                            dispatch (keyword ["=="] [x, y])
                        _ -> bool False) avs bvs
                bool (all (== true) eqs)
            _ -> bool False

    prelude


prelude :: VM ()
prelude = mapM_ eval [$es|
    x max: y :=
      if: (x > y) then: { x } else: { y }

    x min: y :=
      if: (x < y) then: { x } else: { y }
|]
