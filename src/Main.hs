module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Error
import Data.Char (isSpace)
import Prelude hiding (catch)
import System.Console.Haskeline
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath

import Atomo.Environment
import Atomo.Parser
import Atomo.Types


main :: IO ()
main = do
    args <- getArgs

    case args of
        r | null r || r == ["-d"] ->
            exec (repl (r == ["-d"]))

        ("-e":expr:_) -> exec $ do
            ast <- continuedParse expr "<input>"
            r <- evalAll ast
            p <- prettyVM r
            liftIO (print p)

        ("-s":expr:_) -> exec $ do
            ast <- continuedParse expr "<input>"
            evalAll ast
            repl False

        ("-l":fn:_) -> exec $ do
            loadFile False fn
            repl False

        (fn:_) | not (head fn == '-') ->
            exec (loadFile False fn)

        _ -> putStrLn . unlines $
            [ "usage:"
            , "\tatomo\t\tstart the REPL"
            , "\tatomo -d\tstart the REPL in quiet mode"
            , "\tatomo -e EXPR\tevaluate EXPR and output the result"
            , "\tatomo -s EXPR\tevaluate EXPR and start the REPL"
            , "\tatomo -l FILE\tload FILENAME and start the REPL"
            , "\tatomo FILE\texecute FILE"
            ]

repl :: Bool -> VM ()
repl quiet = do
    home <- liftIO getHomeDirectory
    repl' "" $ runInput home . withInterrupt
  where
    escape Interrupt = return Nothing

    runInput home = runInputT defaultSettings
        { historyFile = Just (home </> ".atomo_history")
        }

    repl' input r = do
        me <- liftIO (catch (r $ getInputLine prompt) escape)

        case me of
            Just blank | null (dropWhile isSpace blank) -> repl' input r
            Just part | not (bracesBalanced $ input ++ part) ->
                repl' (input ++ part) r
            Just expr -> do
                catchError
                    (evaluate expr >>= prettyVM >>= liftIO . print)
                    printError

                repl' "" r

            Nothing -> askQuit (repl' input r)
      where
        evaluate expr =
            continuedParse (input ++ expr) "<input>"
                >>= evalAll

        prompt
            | quiet = ""
            | null input = "> "
            | otherwise = ". "

    askQuit continue = do
        r <- liftIO . runInputT defaultSettings $
            getInputChar "really quit? (y/n) "

        case r of
            Just 'y' -> return ()
            Just 'n' -> continue
            _ -> askQuit continue

    bracesBalanced s = hangingBraces s == 0
      where
        hangingBraces :: String -> Int
        hangingBraces [] = 0
        hangingBraces (b:ss)
            | b == '"' = hangingBraces (tail $ dropWhile (/= '"') ss)
            | b == '\'' = hangingBraces (tail $ dropWhile (/= '\'') ss)
            | b `elem` "([{" = 1 + hangingBraces ss
            | b `elem` ")]}" = hangingBraces ss - 1
            | otherwise = hangingBraces ss
