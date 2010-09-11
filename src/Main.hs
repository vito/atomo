module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
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
        ["-e", expr] ->
            exec $ do
                ast <- continuedParse expr "<input>"
                r <- evalAll ast
                p <- prettyVM r
                liftIO (print p)
        [fn] -> do
            source <- readFile fn

            let path = takeDirectory (normalise fn)
            exec $ do
                lift . modify $ \s -> s { loadPath = path:loadPath s }
                ast <- continuedParse source fn
                evalAll ast
                return ()
        {-["-make", fn] -> parseFile fn >>= compileAST-}
        _ -> putStrLn . unlines $
            [ "usage:"
            , "\tatomo\t\tstart the REPL"
            , "\tatomo -d\t\tstart the REPL in quiet mode"
            , "\tatomo -e EXPR\t\tevaluate EXPR and output the result"
            , "\tatomo FILENAME\trun FILENAME"
            ]

repl :: Bool -> VM ()
repl quiet = do
    home <- liftIO getHomeDirectory
    repl' "" $ runInputT
        defaultSettings
            { historyFile = Just (home </> ".atomo_history")
            }
  where
    repl' input r = do
        me <- liftIO . r $ getInputLine $
            if quiet
                then ""
                else if null input then "> " else ". "

        case me of
            Just "" -> repl' input r
            Just part | not (bracesBalanced $ input ++ part) ->
                repl' (input ++ part) r
            Just expr -> do
                res <- fmap Right (continuedParse (input ++ expr) "<input>" >>= evalAll) `catchError` (return . Left)

                case res of
                    Right v -> prettyVM v >>= liftIO . print
                    Left e -> printError e

                repl' "" r
            Nothing -> return ()

    bracesBalanced s = hangingBraces s == 0
      where
        hangingBraces :: String -> Int
        hangingBraces [] = 0
        hangingBraces (b:ss)
            | b `elem` "([{" = 1 + hangingBraces ss
            | b `elem` ")]}" = hangingBraces ss - 1
            | otherwise = hangingBraces ss
