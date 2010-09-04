module Main where

import Control.Monad.Error
import Control.Monad.State
import System.Console.Haskeline
import System.Environment (getArgs, getEnv)
import System.FilePath
import Text.Parsec (ParseError)

import Atomo.Environment
import Atomo.Parser
import Atomo.Pretty
import Atomo.Types


main :: IO ()
main = do
    args <- getArgs

    case args of
        r | null r || r == ["-d"] ->
            exec (repl (r == ["-d"]))
        ["-e", expr] ->
            exec $ do
                r <- evalAST (parseInput expr)
                liftIO (print $ pretty r)
        [fn] -> do
            ast <- parseFile fn

            let path = takeDirectory (normalise fn)
            exec $ do
                modify (\s -> s { loadPath = path:loadPath s })
                evalAST ast
                return ()
        {-["-make", fn] -> parseFile fn >>= compileAST-}
        _ -> putStrLn . unlines $
            [ "usage:"
            , "\tthe\t\tstart the REPL"
            , "\tthe -d\t\tstart the REPL in quiet mode"
            , "\tthe -e EXPR\t\tevaluate EXPR and output the result"
            , "\tthe FILENAME\trun FILENAME"
            ]

repl :: Bool -> VM ()
repl quiet = do
    home <- liftIO $ getEnv "HOME"
    repl' "" $ runInputT
        defaultSettings
            { historyFile = Just (home ++ "/.the_history")
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
                res <- fmap Right (evalAST (parseInput (input ++ expr))) `catchError` (return . Left)

                case res of
                    Right v -> liftIO . print . pretty $ v
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

evalAST :: Either ParseError [Expr] -> VM Value
evalAST (Left e) = throwError $ ParseError e
evalAST (Right ok) = evalAll ok
