{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Ports (load) where

import Data.Char (isSpace)
import Data.Dynamic
import Data.Maybe (catMaybes)
import System.Directory
import System.IO
import qualified Data.ByteString.Char8 as CBS
import qualified Data.Vector as V

import Atomo.Environment
import Atomo.Haskell
import Atomo.Parser
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

    ([$p|current-output-port|] =::) =<< eval [$e|Parameter new: Port standard-output|]
    ([$p|current-input-port|] =::) =<< eval [$e|Parameter new: Port standard-input|]

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

    [$p|(x: Object) print|] =::: [$e|current-output-port _? print: x|]
    [$p|(p: Port) print: x|] =: do
        x <- here "x"
        hdl <- getHandle [$e|p handle|]

        cs <- fmap V.toList $ getList [$e|x as: String|]

        if all isChar cs
            then do
                liftIO (hPutStrLn hdl (map (\(Char c) -> c) cs))
                liftIO (hFlush hdl)
                return x
            else throwError $ ErrorMsg "@as:String returned non-String"

    [$p|(x: Object) display|] =::: [$e|current-output-port _? display: x|]
    [$p|(p: Port) display: x|] =: do
        x <- here "x"
        hdl <- getHandle [$e|p handle|]

        cs <- fmap V.toList $ getList [$e|x as: String|]

        if all isChar cs
            then do
                liftIO (hPutStr hdl (map (\(Char c) -> c) cs))
                liftIO (hFlush hdl)
                return x
            else throwError $ ErrorMsg "@as:String returned non-String"

    [$p|read|] =::: [$e|current-input-port _? read|]
    [$p|(p: Port) read|] =: do
        h <- getHandle [$e|p handle|]

        segment <- liftIO (hGetSegment h)
        parsed <- continuedParse segment "<read>"

        let isPrimitive (Primitive {}) = True
            isPrimitive (EParticle { eParticle = EPMSingle _ }) = True
            isPrimitive (EParticle { eParticle = EPMKeyword _ es }) =
                all isPrimitive (catMaybes es)
            isPrimitive (EList { eContents = es }) = all isPrimitive es
            isPrimitive _ = False

        case parsed of
            [] -> throwError . ErrorMsg $ "impossible: no expressions parsed from input"
            es | all isPrimitive es -> evalAll es
            (e:_) -> return (Expression e)

    [$p|read-line|] =::: [$e|current-input-port _? read-line|]
    [$p|(p: Port) read-line|] =: do
        getHandle [$e|p handle|] >>= liftIO . hGetLine
            >>= string

    [$p|contents|] =::: [$e|current-input-port _? contents|]
    [$p|(p: Port) contents|] =:
        getHandle [$e|p handle|] >>= liftIO . CBS.hGetContents
            >>= string . CBS.unpack

    [$p|(p: Port) flush|] =:
        getHandle [$e|p handle|] >>= liftIO . hFlush
            >> return (particle "ok")

    [$p|(p: Port) close|] =:
        getHandle [$e|p handle|] >>= liftIO . hClose
            >> return (particle "ok")

    [$p|(p: Port) open?|] =:
        getHandle [$e|p handle|] >>= liftIO . hIsOpen
            >>= bool

    [$p|(p: Port) closed?|] =:
        getHandle [$e|p handle|] >>= liftIO . hIsClosed
            >>= bool

    [$p|(p: Port) readable?|] =:
        getHandle [$e|p handle|] >>= liftIO . hIsReadable
            >>= bool

    [$p|(p: Port) writable?|] =:
        getHandle [$e|p handle|] >>= liftIO . hIsWritable
            >>= bool

    [$p|(p: Port) seekable?|] =:
        getHandle [$e|p handle|] >>= liftIO . hIsSeekable
            >>= bool

    [$p|ready?|] =::: [$e|current-input-port _? ready?|]
    [$p|(p: Port) ready?|] =:
        getHandle [$e|p handle|] >>= liftIO . hReady
            >>= bool

    [$p|eof?|] =::: [$e|current-input-port _? eof?|]
    [$p|(p: Port) eof?|] =:
        getHandle [$e|p handle|] >>= liftIO . hIsEOF
            >>= bool


    [$p|File new: (fn: String)|] =::: [$e|Port new: fn|]
    [$p|File open: (fn: String)|] =::: [$e|Port new: fn|]

    [$p|File read: (fn: String)|] =:::
        [$e|Port (new: fn mode: @read) ensuring: @close do: @contents|]

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

    [$p|File make-relative: (fn: String)|] =: do
        fn <- here "fn" >>= findValue isList >>= toString
        liftIO (makeRelativeToCurrentDirectory fn) >>= string

    [$p|File exists?: (fn: String)|] =: do
        fn <- here "fn" >>= findValue isList >>= toString
        liftIO (doesFileExist fn) >>= bool

    [$p|File find-executable: (name: String)|] =: do
        name <- here "name" >>= findValue isList >>= toString
        find <- liftIO (findExecutable name)
        case find of
            Nothing -> return (particle "none")
            Just fn -> do
                str <- string fn
                return (keyParticle ["ok"] [Nothing, Just str])


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

    [$p|Directory contents: (path: String)|] =:
        here "path"
            >>= findValue isList
            >>= toString
            >>= liftIO . getDirectoryContents
            >>= return . filter (not . (`elem` [".", ".."]))
            >>= mapM string
            >>= list

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

    getHandle ex = do
        Haskell hdl <- eval ex
        return (fromDyn hdl (error "handle invalid"))

    hGetSegment :: Handle -> IO String
    hGetSegment h = dropSpaces >> hGetSegment'
      where
        dropSpaces = do
            c <- hLookAhead h
            if isSpace c
                then hGetChar h >> dropSpaces
                else return ()

        hGetSegment' = do
            end <- hIsEOF h

            if end
                then return ""
                else do

            c <- hGetChar h

            case c of
                '"' -> hGetUntil h '"' >>= return . (c:)
                '\'' -> hGetUntil h '\'' >>= return . (c:)
                '(' -> hGetUntil h ')' >>= return . (c:)
                '{' -> hGetUntil h '}' >>= return . (c:)
                '[' -> hGetUntil h ']' >>= return . (c:)
                s | isSpace s -> return [c]
                _ -> do
                    cs <- hGetSegment'
                    return (c:cs)

    hGetUntil :: Handle -> Char -> IO String
    hGetUntil h x = do
        c <- hGetChar h

        if c == x
            then return [c]
            else do
                cs <- hGetUntil h x
                return (c:cs)


prelude :: VM ()
prelude = mapM_ eval [$es|
    with-output-to: (fn: String) do: b :=
        Port (new: fn) ensuring: @close do: { file |
            with-output-to: file do: b
        }

    with-output-to: (p: Port) do: b :=
        with: current-output-port as: p do: b

    with-input-from: (fn: String) do: (b: Block) :=
        Port (new: fn) ensuring: @close do: { file |
            with-input-from: file do: b
        }

    with-input-from: (p: Port) do: (b: Block) :=
        with: current-input-port as: p do: b


    with-all-output-to: (fn: String) do: b :=
        Port (new: fn) ensuring: @close do: { file |
            with-all-output-to: file do: b
        }

    with-all-output-to: (p: Port) do: b :=
        with-default: current-output-port as: p do: b

    with-all-input-from: (fn: String) do: (b: Block) :=
        Port (new: fn) ensuring: @close do: { file |
            with-all-input-from: file do: b
        }

    with-all-input-from: (p: Port) do: (b: Block) :=
        with-default: current-input-port as: p do: b
|]
