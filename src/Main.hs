module Main where

import Control.Monad.Error
import Control.Monad.State
import Data.Hashable (hash)
import Data.Time.Clock.POSIX

import Atomo.Environment
import Atomo.Types


main :: IO ()
main = do
    exec $ do
        mapM_ eval base
        mapM_ eval fibDef

        timings <- replicateM 100 $ do
            b <- liftIO getPOSIXTime
            dispatch . Single (hash "fib") "fib" $ Integer 20
            a <- liftIO getPOSIXTime
            return (a - b)

        liftIO (print (sum timings / 100))

    return ()

base :: [Expr]
base =
    [ Define
        { eLocation = Nothing
        , ePattern = PSingle (hash "print") "print" (PNamed "o" $ PObject (Dispatch Nothing (ESingle (hash "Object") "Object" (ETop Nothing))))
        , eExpr = EVM Nothing $ do
            o <- eval (Dispatch Nothing (ESingle (hash "o") "o" (ETop Nothing)))
            liftIO (print o)
            return (particle "ok")
        }

    , Define
        { eLocation = Nothing
        , ePattern =
            PKeyword
                { ppID = hash ["+"]
                , ppNames = ["+"]
                , ppTargets =
                    [ PNamed "a" $ PObject (Dispatch Nothing (ESingle (hash "Integer") "Integer" (ETop Nothing)))
                    , PNamed "b" $ PObject (Dispatch Nothing (ESingle (hash "Integer") "Integer" (ETop Nothing)))
                    ]
                }
        , eExpr = EVM Nothing $ do
            Integer a <- eval (Dispatch Nothing (ESingle (hash "a") "a" (ETop Nothing)))
            Integer b <- eval (Dispatch Nothing (ESingle (hash "b") "b" (ETop Nothing)))
            return (Integer (a + b))
        }

    , Define
        { eLocation = Nothing
        , ePattern =
            PKeyword
                { ppID = hash ["-"]
                , ppNames = ["-"]
                , ppTargets =
                    [ PNamed "a" $ PObject (Dispatch Nothing (ESingle (hash "Integer") "Integer" (ETop Nothing)))
                    , PNamed "b" $ PObject (Dispatch Nothing (ESingle (hash "Integer") "Integer" (ETop Nothing)))
                    ]
                }
        , eExpr = EVM Nothing $ do
            Integer a <- eval (Dispatch Nothing (ESingle (hash "a") "a" (ETop Nothing)))
            Integer b <- eval (Dispatch Nothing (ESingle (hash "b") "b" (ETop Nothing)))
            return (Integer (a - b))
        }
    ]

basic :: [Expr]
basic = base ++
    [ Set
        { eLocation = Nothing
        , ePattern = PNamed "x" PAny
        , eExpr = Primitive Nothing (Integer 42)
        }
    , Dispatch Nothing (ESingle (hash "x") "x" (ETop Nothing))
    , Dispatch Nothing (ESingle (hash "print") "print" $ Dispatch Nothing (ESingle (hash "x") "x" (ETop Nothing)))
    ]

fibDef :: [Expr]
fibDef =
    [ Define
        { eLocation = Nothing
        , ePattern = PSingle (hash "fib") "fib" (PMatch (Integer 0))
        , eExpr = Primitive Nothing (Integer 1)
        }

    , Define
        { eLocation = Nothing
        , ePattern = PSingle (hash "fib") "fib" (PMatch (Integer 1))
        , eExpr = Primitive Nothing (Integer 1)
        }

    , Define
        { eLocation = Nothing
        , ePattern = PSingle (hash "fib") "fib" $
            (PNamed "n"
                (PObject
                    (Dispatch
                        { eLocation = Nothing
                        , eMessage = ESingle (hash "Integer") "Integer" (ETop Nothing)
                        })))
        , eExpr =
            Dispatch
                { eLocation = Nothing
                , eMessage =
                    EKeyword (hash ["+"]) ["+"]
                        [ Dispatch
                            { eLocation = Nothing
                            , eMessage = ESingle (hash "fib") "fib" $
                                Dispatch
                                    { eLocation = Nothing
                                    , eMessage = EKeyword (hash ["-"]) ["-"] [Dispatch Nothing (ESingle (hash "n") "n" (ETop Nothing)), Primitive Nothing $ Integer 2]
                                    }
                            }

                        , Dispatch
                            { eLocation = Nothing
                            , eMessage = ESingle (hash "fib") "fib" $
                                Dispatch
                                    { eLocation = Nothing
                                    , eMessage = EKeyword (hash ["-"]) ["-"] [Dispatch Nothing (ESingle (hash "n") "n" (ETop Nothing)), Primitive Nothing $ Integer 1]
                                    }
                            }
                        ]
                }
        }
    ]

fib :: [Expr]
fib =
    [ Dispatch Nothing (ESingle (hash "fib") "fib" (Primitive Nothing (Integer 20)))
    ]
