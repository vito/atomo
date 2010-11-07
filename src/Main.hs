{-# LANGUAGE QuasiQuotes #-}
module Main where

import "monads-fd" Control.Monad.Cont
import System.Environment (getArgs)

import Atomo
import Atomo.Load
import Atomo.Parser
import Atomo.PrettyVM
import Atomo.Run


main :: IO ()
main = do
    args <- getArgs

    case args of
        [] -> exec repl

        ("-e":expr:_) -> exec $ do
            ast <- continuedParse expr "<input>"
            r <- evalAll ast
            d <- prettyVM r
            liftIO (putStrLn d)
            return (particle "ok")

        ("-s":expr:_) -> exec $ do
            ast <- continuedParse expr "<input>"
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

repl :: VM Value
repl = eval [$e|Lobby clone repl|]
