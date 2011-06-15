{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Environment where

import System.Environment

import Atomo


load :: VM ()
load = do
    ([p|Environment|] =::) =<< eval [e|Object clone|]

    [p|Environment arguments|] =:
        liftM (list . map string) (liftIO getArgs)

    [p|Environment program-name|] =:
        liftM string $ liftIO getProgName

    [p|Environment get: (name: String)|] =:
        getString [e|name|]
            >>= liftM string . liftIO . getEnv

    [p|Environment all|] =: do
        env <- liftIO getEnvironment

        assocs <- forM env $ \(k, v) ->
            dispatch (keyword ["->"] [string k, string v])

        return $ list assocs
