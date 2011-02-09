module Atomo.Load where

import Control.Monad.State
import System.Directory
import System.FilePath
import qualified Language.Haskell.Interpreter as H

import Atomo.Environment
import Atomo.Parser
import Atomo.Types


-- | Load a file, remembering its absolute path to prevent repeated loading.
-- Searches with cwd as lowest priority
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

-- | Load a file. Searches with cwd as highest priority.
loadFile :: FilePath -> VM Value
loadFile fn = do
    initialPath <- gets loadPath
    findFile ("":initialPath) fn >>= doLoad

-- | Execute a file; for .hs filenames, interprets its `load' function and
-- calls it. for anything else, it interprets it as Atomo source.
doLoad :: FilePath -> VM Value
doLoad file =
    case takeExtension file of
        ".hs" -> do
            int <- liftIO . H.runInterpreter $ do
                H.loadModules [file]
                H.setTopLevelModules ["Main"]
                H.interpret "load" (H.as :: VM ())

            join (either (throwError . ImportError) return int)

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

-- | Given a list of paths to search, find the file to load. Attempts to find
-- the filename with .atomo and .hs extensions.
--
-- If no file is found, throws a FileNotFound error.
findFile :: [FilePath] -> FilePath -> VM FilePath
findFile [] fn = throwError (FileNotFound fn)
findFile (p:ps) fn = do
    check <- filterM (liftIO . doesFileExist . ((p </> fn) <.>)) exts

    case check of
        [] -> findFile ps fn
        (ext:_) -> liftIO (canonicalizePath $ p </> fn <.> ext)
  where
    exts = ["", "atomo", "hs"]

