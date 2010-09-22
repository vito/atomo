{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Concurrency (load) where

import qualified Data.IntMap as M
import qualified Data.Vector as V

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
        Process chan _ <- here "p" >>= findValue isProcess
        v <- here "v"
        liftIO (writeChan chan v)
        here "p"

    [$p|(b: Block) spawn|] =: do
        Block s as bes <- here "b" >>= findValue isBlock

        if length as > 0
            then throwError (BlockArity (length as) 0)
            else do
                st <- lift get
                chan <- liftIO newChan
                tid <- liftIO $ forkIO (runWith (go $ doBlock M.empty s bes >> return ()) (st { channel = chan }) >> return ())
                return (Process chan tid)

    [$p|(b: Block) spawn: (l: List)|] =: do
        Block s as bes <- here "b" >>= findValue isBlock
        vs <- fmap V.toList $ getList [$e|l|]

        if length as > length vs
            then throwError (BlockArity (length as) (length vs))
            else do
                st <- lift get
                chan <- liftIO newChan
                tid <- liftIO . forkIO $ do
                    runWith
                        (go $ doBlock (toMethods . concat $ zipWith bindings' as vs) s bes >> return ())
                        (st { channel = chan })

                    return ()
                return (Process chan tid)

    [$p|(p: Process) stop|] =: do
        Process _ tid <- here "p" >>= findValue isProcess
        liftIO (killThread tid)
        return (particle "ok")
