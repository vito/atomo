{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Concurrency (load) where

import System.Timeout

import Atomo
import Atomo.Method
import Atomo.Spawn
import Atomo.Valuable


load :: VM ()
load = do
    [p|self|] =: do
        chan <- gets channel
        tid <- liftIO myThreadId
        return (Process chan tid)

    [p|receive|] =: gets channel >>= liftIO . readChan

    [p|receive-timeout: (n: Integer)|] =: do
        Integer t <- here "n" >>= findInteger
        c <- gets channel
        liftIO (timeout' t (readChan c)) >>= toValue

    [p|halt|] =: gets halt >>= liftIO >> return (particle "ok")

    [p|(p: Process) <- v|] =: do
        Process chan _ <- here "p" >>= findProcess
        v <- here "v"
        liftIO (writeChan chan v)
        here "p"

    [p|(b: Block) spawn|] =: do
        Block s as bes <- here "b" >>= findBlock

        if length as > 0
            then throwError (BlockArity (length as) 0)
            else spawn (doBlock emptyMap s bes)

    [p|(b: Block) spawn: (... args)|] =: do
        b@(Block _ as _) <- here "b" >>= findBlock
        vs <- getList [e|args|]

        if length as > length vs
            then throwError (BlockArity (length as) (length vs))
            else spawn (callBlock b vs)

    [p|(p: Process) stop|] =: do
        Process _ tid <- here "p" >>= findProcess
        liftIO (killThread tid)
        return (particle "ok")

timeout' :: Integer -> IO a -> IO (Maybe a)
timeout' n x
    | n > fromIntegral limit = do
        mr <- timeout (fromIntegral n) x
        case mr of
            Nothing -> timeout' (n - fromIntegral limit) x
            Just r -> return (Just r)
    | otherwise = timeout (fromIntegral n) x
  where
    limit = maxBound :: Int