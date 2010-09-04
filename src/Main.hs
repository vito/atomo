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
        ["-e", expr] -> do
            run $ do
                r <- evalAST (parseInput expr) `catchError` (return . Left)
                case r of
                    Left e -> liftIO (print $ pretty e)
                    Right r -> liftIO (print r)

            return ()
        [fn] -> do
            ast <- parseFile fn

            let path = takeDirectory (normalise fn)
            r <- run $ do
                modify (\s -> s { loadPath = path:loadPath s })
                evalAST ast
                return ()

            case r of
                Left e -> print $ pretty e
                Right _ -> return ()
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
                res <- evalAST (parseInput (input ++ expr)) `catchError` (return . Left)

                case res of
                    Right v -> liftIO . print . pretty $ v
                    Left e -> liftIO . print . pretty $ e

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

evalAST :: Either ParseError [Expr] -> VM (Either AtomoError Value)
evalAST (Left e) = return . Left $ ParseError e
evalAST (Right ok) = fmap Right $ evalAll ok
