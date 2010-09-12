{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Ports (load) where

import Data.Dynamic
import System.Directory
import System.IO
import qualified Data.Vector as V

import Atomo.Environment
import Atomo.Haskell
import Atomo.Pretty


load :: VM ()
load = do
    ([$p|Port|] =::) =<< eval [$e|Object clone|]
    ([$p|File|] =::) =<< eval [$e|Object clone|]
    ([$p|Directory|] =::) =<< eval [$e|Object clone|]

    sinp <- portObj stdin
    soutp <- portObj stdout
    serrp <- portObj stderr
    [$p|Port standard-input|] =:: sinp
    [$p|Port standard-output|] =:: soutp
    [$p|Port standard-error|] =:: serrp

    [$p|current-output-port|] =:: soutp
    [$p|current-input-port|] =:: sinp

    [$p|Port new: (fn: String)|] =::: [$e|Port new: fn mode: @read-write|]
    [$p|Port new: (fn: String) mode: (m: Particle)|] =: do
        fn <- fmap (map (\(Char c) -> c) . V.toList) (getList [$e|fn|])
        Particle m <- here "m" >>= findValue isParticle

        hdl <- case m of
            PMSingle "read" ->
                liftIO (openFile fn ReadMode)
            PMSingle "write" ->
                liftIO (openFile fn WriteMode)
            PMSingle "append" ->
                liftIO (openFile fn AppendMode)
            PMSingle "read-write" ->
                liftIO (openFile fn ReadWriteMode)
            _ ->
                error $ "unknown port mode: " ++ show (pretty m) ++ ", must be one of: @read, @write, @append, @read-write"

        portObj hdl

    [$p|(p: Port) flush|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hFlush (fromDyn hdl (error "port handle invalid!"))) -- TODO
        return (particle "ok")

    [$p|(p: Port) close|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hClose (fromDyn hdl (error "port handle invalid!"))) -- TODO
        return (particle "ok")

    [$p|(p: Port) open?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hIsOpen (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(p: Port) readable?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hIsReadable (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(p: Port) writable?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hIsWritable (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(p: Port) seekable?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hIsSeekable (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(p: Port) closed?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hIsClosed (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(p: Port) ready?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hReady (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(p: Port) eof?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hIsEOF (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(x: Object) print|] =: do
        x <- here "x"
        Haskell h <- eval [$e|dispatch sender current-output-port handle|]

        cs <- fmap V.toList $ getList [$e|x as: String|]

        let hdl = fromDyn h (error "current-output-port handle invalid!")

        if all isChar cs
            then do
                liftIO (hPutStrLn hdl (map (\(Char c) -> c) cs))
                liftIO (hFlush hdl)
                return x
            else throwError $ ErrorMsg "@as:String returned non-String"

    [$p|read-line|] =: do
        Haskell inh <- eval [$e|dispatch sender current-input-port handle|]
        line <- liftIO (hGetLine (fromDyn inh (error "current-input-port handle invalid!"))) -- TODO
        string line

    [$p|File new: (fn: String)|] =::: [$e|Port new: fn|]
    [$p|File open: (fn: String)|] =::: [$e|Port new: fn|]

    [$p|File delete: (fn: String)|] =: do
        fn <- here "fn" >>= findValue isList >>= toString
        liftIO (removeFile fn)
        return (particle "ok")

    [$p|File move: (from: String) to: (to: String)|] =:::
        [$e|File rename: from to: to|]
    [$p|File rename: (from: String) to: (to: String)|] =: do
        from <- here "from" >>= findValue isList >>= toString
        to <- here "to" >>= findValue isList >>= toString
        liftIO (renameFile from to)
        return (particle "ok")

    [$p|File copy: (from: String) to: (to: String)|] =: do
        from <- here "from" >>= findValue isList >>= toString
        to <- here "to" >>= findValue isList >>= toString
        liftIO (copyFile from to)
        return (particle "ok")

    [$p|File canonicalize-path: (fn: String)|] =: do
        fn <- here "fn" >>= findValue isList >>= toString
        liftIO (canonicalizePath fn) >>= string

    [$p|File exists?: (fn: String)|] =: do
        fn <- here "fn" >>= findValue isList >>= toString
        liftIO (doesFileExist fn) >>= bool

    [$p|Directory create: (path: String)|] =: do
        path <- here "path" >>= findValue isList >>= toString
        liftIO (createDirectory path)
        return (particle "ok")

    [$p|Directory create-if-missing: (path: String)|] =: do
        path <- here "path" >>= findValue isList >>= toString
        liftIO (createDirectoryIfMissing False path)
        return (particle "ok")

    [$p|Directory create-tree-if-missing: (path: String)|] =: do
        path <- here "path" >>= findValue isList >>= toString
        liftIO (createDirectoryIfMissing True path)
        return (particle "ok")

    [$p|Directory remove: (path: String)|] =: do
        path <- here "path" >>= findValue isList >>= toString
        liftIO (removeDirectory path)
        return (particle "ok")

    [$p|Directory remove-recursive: (path: String)|] =: do
        path <- here "path" >>= findValue isList >>= toString
        liftIO (removeDirectoryRecursive path)
        return (particle "ok")

    [$p|Directory move: (from: String) to: (to: String)|] =:::
        [$e|Directory rename: from to: to|]
    [$p|Directory rename: (from: String) to: (to: String)|] =: do
        from <- here "from" >>= findValue isList >>= toString
        to <- here "to" >>= findValue isList >>= toString
        liftIO (renameDirectory from to)
        return (particle "ok")

    [$p|Directory contents: (path: String)|] =: do
        path <- here "path" >>= findValue isList >>= toString
        liftIO (getDirectoryContents path) >>= mapM string >>= list

    [$p|Directory current|] =:
        liftIO getCurrentDirectory >>= string

    [$p|Directory current: (path: String)|] =: do
        path <- here "path" >>= findValue isList >>= toString
        liftIO (setCurrentDirectory path)
        return (particle "ok")

    [$p|Directory home|] =:
        liftIO getHomeDirectory >>= string

    [$p|Directory user-data-for: (app: String)|] =: do
        app <- here "app" >>= findValue isList >>= toString
        liftIO (getAppUserDataDirectory app) >>= string

    [$p|Directory user-documents|] =:
        liftIO getUserDocumentsDirectory >>= string

    [$p|Directory temporary|] =:
        liftIO getTemporaryDirectory >>= string

    [$p|Directory exists?: (path: String)|] =: do
        path <- here "path" >>= findValue isList >>= toString
        liftIO (doesDirectoryExist path) >>= bool

    prelude
  where
    portObj hdl = newScope $ do
        port <- eval [$e|Port clone|]
        [$p|p|] =:: port
        [$p|p handle|] =:: Haskell (toDyn hdl)
        here "p"


prelude :: VM ()
prelude = mapM_ eval [$es|
    with-output-to: (fn: String) do: b :=
        Port (new: fn) ensuring: @close do: { file |
            with-output-to: file do: b
        }

    with-output-to: (p: Port) do: b := {
        current-output-port = p
        join: b
    } call

    with-input-from: (fn: String) do: (b: Block) :=
        Port (new: fn) ensuring: @close do: { file |
            with-input-from: file do: b
        }

    with-input-from: (p: Port) do: (b: Block) := {
        current-input-port = p
        join: b
    } call
|]
