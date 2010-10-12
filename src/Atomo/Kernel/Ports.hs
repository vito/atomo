{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Ports (load) where

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import System.Directory
import System.FilePath ((</>), (<.>))
import System.IO
import qualified Data.Text.IO as TIO

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
        fn <- getString [$e|fn|]
        Particle m <- here "m" >>= findParticle

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

        String s <- eval [$e|x as: String|] >>= findString

        liftIO (TIO.hPutStrLn hdl s)
        liftIO (hFlush hdl)
        return x

    [$p|(x: Object) display|] =::: [$e|current-output-port _? display: x|]
    [$p|(p: Port) display: x|] =: do
        x <- here "x"
        hdl <- getHandle [$e|p handle|]

        String s <- eval [$e|x as: String|] >>= findString

        liftIO (TIO.hPutStr hdl s)
        liftIO (hFlush hdl)
        return x

    [$p|read|] =::: [$e|current-input-port _? read|]
    [$p|(p: Port) read|] =: do
        h <- getHandle [$e|p handle|]

        segment <- liftIO (hGetSegment h)
        parsed <- continuedParse segment "<read>"

        let isPrimitive (Primitive {}) = True
            isPrimitive (EParticle { eParticle = EPMSingle _ }) = True
            isPrimitive (EParticle { eParticle = EPMKeyword _ ts }) =
                all isPrimitive (catMaybes ts)
            isPrimitive (EList { eContents = ts }) = all isPrimitive ts
            isPrimitive _ = False

        case parsed of
            [] -> raise' "no-expressions-parsed"
            is | all isPrimitive is -> evalAll is
            (i:_) -> return (Expression i)

    [$p|read-line|] =::: [$e|current-input-port _? read-line|]
    [$p|(p: Port) read-line|] =: do
        h <- getHandle [$e|p handle|]
        done <- liftIO (hIsEOF h)

        if done
            then raise' "end-of-input"
            else fmap String $ liftIO (TIO.hGetLine h)

    [$p|read-char|] =::: [$e|current-input-port _? read-char|]
    [$p|(p: Port) read-char|] =: do
        h <- getHandle [$e|p handle|]
        b <- liftIO (hGetBuffering h)
        liftIO (hSetBuffering h NoBuffering)
        c <- liftIO (hGetChar h)
        liftIO (hSetBuffering h b)

        return (Char c)

    [$p|contents|] =::: [$e|current-input-port _? contents|]
    [$p|(p: Port) contents|] =:
        getHandle [$e|p handle|] >>= liftIO . TIO.hGetContents
            >>= return . String

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
        fn <- getString [$e|fn|]
        liftIO (removeFile fn)
        return (particle "ok")

    [$p|File move: (from: String) to: (to: String)|] =:::
        [$e|File rename: from to: to|]
    [$p|File rename: (from: String) to: (to: String)|] =: do
        from <- getString [$e|from|]
        to <- getString [$e|to|]
        liftIO (renameFile from to)
        return (particle "ok")

    [$p|File copy: (from: String) to: (to: String)|] =: do
        from <- getString [$e|from|]
        to <- getString [$e|to|]
        liftIO (copyFile from to)
        return (particle "ok")

    [$p|File canonicalize-path: (fn: String)|] =: do
        fn <- getString [$e|fn|]
        fmap string $ liftIO (canonicalizePath fn)

    [$p|File make-relative: (fn: String)|] =: do
        fn <- getString [$e|fn|]
        fmap string $ liftIO (makeRelativeToCurrentDirectory fn)

    [$p|File exists?: (fn: String)|] =: do
        fn <- getString [$e|fn|]
        liftIO (doesFileExist fn) >>= bool

    [$p|File find-executable: (name: String)|] =: do
        name <- getString [$e|name|]
        find <- liftIO (findExecutable name)
        case find of
            Nothing -> return (particle "none")
            Just fn -> return (keyParticle ["ok"] [Nothing, Just (string fn)])

    [$p|File readable?: (fn: String)|] =:
        getString [$e|fn|]
            >>= fmap readable . liftIO . getPermissions
            >>= bool

    [$p|File writable?: (fn: String)|] =:
        getString [$e|fn|]
            >>= fmap writable . liftIO . getPermissions
            >>= bool

    [$p|File executable?: (fn: String)|] =:
        getString [$e|fn|]
            >>= fmap executable . liftIO . getPermissions
            >>= bool

    [$p|File searchable?: (fn: String)|] =:
        getString [$e|fn|]
            >>= fmap searchable . liftIO . getPermissions
            >>= bool

    [$p|File set-readable: (fn: String) to: (b: Boolean)|] =: do
        t <- bool True
        b <- here "b"
        fn <- getString [$e|fn|]
        ps <- liftIO (getPermissions fn)
        liftIO (setPermissions fn (ps { readable = b == t }))
        return (particle "ok")

    [$p|File set-writable: (fn: String) to: (b: Boolean)|] =: do
        t <- bool True
        b <- here "b"
        fn <- getString [$e|fn|]
        ps <- liftIO (getPermissions fn)
        liftIO (setPermissions fn (ps { writable = b == t }))
        return (particle "ok")

    [$p|File set-executable: (fn: String) to: (b: Boolean)|] =: do
        t <- bool True
        b <- here "b"
        fn <- getString [$e|fn|]
        ps <- liftIO (getPermissions fn)
        liftIO (setPermissions fn (ps { executable = b == t }))
        return (particle "ok")

    [$p|File set-searchable: (fn: String) to: (b: Boolean)|] =: do
        t <- bool True
        b <- here "b"
        fn <- getString [$e|fn|]
        ps <- liftIO (getPermissions fn)
        liftIO (setPermissions fn (ps { searchable = b == t }))
        return (particle "ok")

    [$p|Directory create: (path: String)|] =: do
        path <- getString [$e|path|]
        liftIO (createDirectory path)
        return (particle "ok")

    [$p|Directory create-if-missing: (path: String)|] =: do
        path <- getString [$e|path|]
        liftIO (createDirectoryIfMissing False path)
        return (particle "ok")

    [$p|Directory create-tree-if-missing: (path: String)|] =: do
        path <- getString [$e|path|]
        liftIO (createDirectoryIfMissing True path)
        return (particle "ok")

    [$p|Directory remove: (path: String)|] =: do
        path <- getString [$e|path|]
        liftIO (removeDirectory path)
        return (particle "ok")

    [$p|Directory remove-recursive: (path: String)|] =: do
        path <- getString [$e|path|]
        liftIO (removeDirectoryRecursive path)
        return (particle "ok")

    [$p|Directory move: (from: String) to: (to: String)|] =:::
        [$e|Directory rename: from to: to|]
    [$p|Directory rename: (from: String) to: (to: String)|] =: do
        from <- getString [$e|from|]
        to <- getString [$e|to|]
        liftIO (renameDirectory from to)
        return (particle "ok")

    [$p|Directory copy: (from: String) to: (to: String)|] =::: [$e|{
        Directory create-tree-if-missing: to

        Directory (contents: from) map: { c |
            f = from / c
            t = to / c

            if: Directory (exists?: f)
                then: { Directory copy: f to: t }
                else: { File copy: f to: t }
        }

        @ok
    } call|]

    [$p|Directory contents: (path: String)|] =:
        getString [$e|path|]
            >>= liftIO . getDirectoryContents
            >>= return . filter (not . (`elem` [".", ".."]))
            >>= list . map string

    [$p|Directory current|] =:
        fmap string $ liftIO getCurrentDirectory

    [$p|Directory current: (path: String)|] =: do
        path <- getString [$e|path|]
        liftIO (setCurrentDirectory path)
        return (particle "ok")

    [$p|Directory home|] =:
        fmap string $ liftIO getHomeDirectory

    [$p|Directory user-data-for: (app: String)|] =: do
        app <- getString [$e|app|]
        fmap string $ liftIO (getAppUserDataDirectory app)

    [$p|Directory user-documents|] =:
        fmap string $ liftIO getUserDocumentsDirectory

    [$p|Directory temporary|] =:
        fmap string $ liftIO getTemporaryDirectory

    [$p|Directory exists?: (path: String)|] =: do
        path <- getString [$e|path|]
        liftIO (doesDirectoryExist path) >>= bool

    [$p|(a: String) </> (b: String)|] =: do
        a <- getString [$e|a|]
        b <- getString [$e|b|]
        return (string (a </> b))

    [$p|(a: String) <.> (b: String)|] =: do
        a <- getString [$e|a|]
        b <- getString [$e|b|]
        return (string (a <.> b))

    prelude
  where
    portObj hdl = newScope $ do
        port <- eval [$e|Port clone|]
        [$p|p|] =:: port
        [$p|p handle|] =:: haskell hdl
        here "p"

    getHandle ex = eval ex >>= fromHaskell "Handle"

    hGetSegment :: Handle -> IO String
    hGetSegment h = dropSpaces >> hGetSegment' Nothing
      where
        dropSpaces = do
            c <- hLookAhead h
            if isSpace c
                then hGetChar h >> dropSpaces
                else return ()

        hGetSegment' stop = do
            end <- hIsEOF h

            if end
                then return ""
                else do

            c <- hGetChar h

            case c of
                '"' -> hGetUntil h '"' >>= return . (c:)
                '\'' -> hGetUntil h '\'' >>= return . (c:)
                '(' -> nested '(' ')'
                '{' -> nested '{' '}'
                '[' -> nested '[' ']'
                s | (stop == Nothing && isSpace s) || (Just s == stop) ->
                    return [c]
                _ -> do
                    cs <- hGetSegment' stop
                    return (c:cs)
          where
            nested c end = do
                sub <- fmap (c:) $ hGetSegment' (Just end)
                rest <- hGetSegment' stop
                return (sub ++ rest)

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
        { Port (new: fn mode: @write) } wrap: @close do: { file |
            with-output-to: file do: b
        }

    with-output-to: (p: Port) do: b :=
        with: current-output-port as: p do: b

    with-input-from: (fn: String) do: (b: Block) :=
        { Port (new: fn mode: @read) } wrap: @close do: { file |
            with-input-from: file do: b
        }

    with-input-from: (p: Port) do: (b: Block) :=
        with: current-input-port as: p do: b


    with-all-output-to: (fn: String) do: b :=
        { Port (new: fn mode: @write) } wrap: @close do: { file |
            with-all-output-to: file do: b
        }

    with-all-output-to: (p: Port) do: b :=
        with-default: current-output-port as: p do: b

    with-all-input-from: (fn: String) do: (b: Block) :=
        { Port (new: fn mode: @read) } wrap: @close do: { file |
            with-all-input-from: file do: b
        }

    with-all-input-from: (p: Port) do: (b: Block) :=
        with-default: current-input-port as: p do: b
|]
