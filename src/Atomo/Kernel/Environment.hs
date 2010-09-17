{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Environment where

import System.Environment

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    ([$p|Environment|] =::) =<< eval [$e|Object clone|]

    [$p|Environment arguments|] =:
        liftIO getArgs >>= mapM string >>= list

    [$p|Environment program-name|] =:
        liftIO getProgName >>= string

    [$p|Environment get: (name: String)|] =:
        here "name"
            >>= findValue isList
            >>= toString
            >>= liftIO . getEnv
            >>= string

    [$p|Environment all|] =: do
        env <- liftIO getEnvironment

        assocs <- forM env $ \(k, v) -> do
            f <- string k
            t <- string v
            dispatch (keyword ["->"] [f, t])

        list assocs
