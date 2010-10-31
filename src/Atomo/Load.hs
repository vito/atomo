module Atomo.Load where

import "monads-fd" Control.Monad.State
import System.Directory
import System.FilePath
import qualified Language.Haskell.Interpreter as H

import Atomo.Environment
import Atomo.Parser
import Atomo.Types


-- load a file, remembering it to prevent repeated loading
-- searches with cwd as lowest priority
requireFile :: FilePath -> VM Value
requireFile fn = do
    initialPath <- gets loadPath
    file <- findFile (initialPath ++ [""]) fn

    alreadyLoaded <- gets ((file `elem`) . loaded)
    if alreadyLoaded
        then return (particle "already-loaded")
        else do

    modify $ \s -> s { loaded = file : loaded s }

    doLoad file

-- load a file
-- searches with cwd as highest priority
loadFile :: FilePath -> VM Value
loadFile fn = do
    initialPath <- gets loadPath
    findFile ("":initialPath) fn >>= doLoad

-- execute a file
doLoad :: FilePath -> VM Value
doLoad file =
    case takeExtension file of
        ".hs" -> do
            int <- liftIO . H.runInterpreter $ do
                H.loadModules [file]
                H.setTopLevelModules ["Main"]
                H.interpret "load" (H.as :: VM ())

            load <- either (throwError . ImportError) return int

            load

            return (particle "ok")

        _ -> do
            initialPath <- gets loadPath

            ast <- parseFile file

            modify $ \s -> s
                { loadPath = [takeDirectory file]
                }

            r <- evalAll ast

            modify $ \s -> s
                { loadPath = initialPath
                }

            return r

-- | given a list of paths to search, find the file to load
-- attempts to find the filename with .atomo and .hs extensions
findFile :: [FilePath] -> FilePath -> VM FilePath
findFile [] fn = throwError (FileNotFound fn)
findFile (p:ps) fn = do
    check <- filterM (liftIO . doesFileExist . ((p </> fn) <.>)) exts

    case check of
        [] -> findFile ps fn
        (ext:_) -> liftIO (canonicalizePath $ p </> fn <.> ext)
  where
    exts = ["", "atomo", "hs"]

