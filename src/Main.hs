{-# LANGUAGE QuasiQuotes #-}
module Main where

import "monads-fd" Control.Monad.Cont
import System.Environment (getArgs)

import Atomo
import Atomo.Core
import Atomo.Load
import Atomo.Parser
import Atomo.PrettyVM
import Atomo.Run
import qualified Atomo.Kernel as Kernel


main :: IO ()
main = do
    args <- getArgs

    case args of
        [] -> exec repl

        ["-x"] -> noPrelude primRepl
        ("-x":fn:_) -> noPrelude (parseFile fn >>= liftIO . mapM_ print >> liftIO (putStrLn "result:") >> loadFile fn)

        ("-e":expr:_) -> exec $ do
            ast <- parseInput expr
            r <- evalAll ast
            d <- prettyVM r
            liftIO (putStrLn d)
            return (particle "ok")

        ("-s":expr:_) -> exec $ do
            ast <- parseInput expr
            evalAll ast
            repl

        ("-l":fn:_) -> exec $ do
            loadFile fn
            repl

        (fn:_) | head fn /= '-' ->
            exec (loadFile fn)

        _ -> putStrLn . unlines $
            [ "usage:"
            , "\tatomo\t\tstart the REPL"
            , "\tatomo -e EXPR\tevaluate EXPR and output the result"
            , "\tatomo -s EXPR\tevaluate EXPR and start the REPL"
            , "\tatomo -l FILE\tload FILENAME and start the REPL"
            , "\tatomo FILE\texecute FILE"
            ]

noPrelude :: VM Value -> IO ()
noPrelude x = do
    runWith (initCore >> Kernel.load >> x) startEnv
    return ()

repl :: VM Value
repl = eval [$e|Lobby clone repl|]

primRepl :: VM Value
primRepl = do
    liftIO (putStrLn "next:")
    expr <- liftIO getLine
    ast <- parseInput expr
    r <- evalAll ast
    d <- prettyVM r
    liftIO (putStrLn d)
    primRepl
