{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Concurrency (load) where

import qualified Data.IntMap as M

import Atomo.Environment
import Atomo.Haskell
import Atomo.Method


load :: VM ()
load = do
    [$p|self|] =: do
        chan <- lift (gets channel)
        tid <- liftIO myThreadId
        return (Process chan tid)

    [$p|receive|] =: do
        chan <- lift (gets channel)
        v <- liftIO (readChan chan)
        return v

    [$p|halt|] =: lift (gets halt) >>= liftIO >> return (particle "ok")

    [$p|(p: Process) ! v|] =: do
        Process chan _ <- here "p" >>= findProcess
        v <- here "v"
        liftIO (writeChan chan v)
        here "p"

    [$p|(b: Block) spawn|] =: do
        Block s as bes <- here "b" >>= findBlock

        if length as > 0
            then throwError (BlockArity (length as) 0)
            else spawn (doBlock M.empty s bes)

    [$p|(b: Block) spawn: (l: List)|] =: do
        Block s as bes <- here "b" >>= findBlock
        vs <- getList [$e|l|]

        if length as > length vs
            then throwError (BlockArity (length as) (length vs))
            else spawn $
                doBlock (toMethods . concat $ zipWith bindings' as vs) s bes

    [$p|(p: Process) stop|] =: do
        Process _ tid <- here "p" >>= findProcess
        liftIO (killThread tid)
        return (particle "ok")
