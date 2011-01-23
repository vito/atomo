{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Ports (load) where

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import System.Console.Haskeline as Haskeline
import System.Directory
import System.FilePath ((</>), (<.>))
import System.IO
import qualified Data.Text.IO as TIO

import Atomo
import Atomo.Parser
import Atomo.Pretty
import Atomo.Valuable


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

    [$p|(p: Port) show|] =: do
        hdl <- getHandle [$e|p handle|] >>= liftIO . hShow
        return (string ("<port " ++ hdl ++ ">"))

    [$p|Port new: (fn: String) &mode: @read-write|] =: do
        fn <- getString [$e|fn|]
        Particle m <- here "mode" >>= findParticle

        hdl <- case m of
            Single { mName = "read" } -> do
                checkExists fn
                liftIO (openFile fn ReadMode)
            Single { mName = "write" } ->
                liftIO (openFile fn WriteMode)
            Single { mName = "append" } ->
                liftIO (openFile fn AppendMode)
            Single { mName = "read-write" } ->
                liftIO (openFile fn ReadWriteMode)
            _ ->
                error $ "unknown port mode: " ++ show (pretty m) ++ ", must be one of: @read, @write, @append, @read-write"

        portObj hdl

    [$p|(p: Port) buffering|] =:
        getHandle [$e|p handle|] >>= liftIO . hGetBuffering >>= toValue

    [$p|(p: Port) set-buffering: mode|] =: do
        h <- getHandle [$e|p handle|]
        m <- here "mode" >>= fromValue
        liftIO (hSetBuffering h m)
        return (particle "ok")

    [$p|(p: Port) print: x|] =: do
        x <- here "x"
        port <- here "p"
        hdl <- getHandle [$e|p handle|]

        c <- liftIO (hIsClosed hdl)
        when c (raise ["port-closed", "for"] [port, x])

        String s <- eval [$e|x as: String|] >>= findString

        liftIO (TIO.hPutStrLn hdl s)
        liftIO (hFlush hdl)
        return x

    [$p|(p: Port) display: x|] =: do
        x <- here "x"
        port <- here "p"
        hdl <- getHandle [$e|p handle|]

        c <- liftIO (hIsClosed hdl)
        when c (raise ["port-closed", "for"] [port, x])

        String s <- eval [$e|x as: String|] >>= findString

        liftIO (TIO.hPutStr hdl s)
        liftIO (hFlush hdl)
        return x

    [$p|(p: Port) read|] =: do
        h <- getHandle [$e|p handle|]

        segment <- liftIO (hGetSegment h)
        parsed <- continuedParse segment "<read>"

        let isPrimitive (EPrimitive {}) = True
            isPrimitive (EParticle { eParticle = Single {} }) = True
            isPrimitive (EParticle { eParticle = Keyword { mTargets = ts } }) =
                all isPrimitive (catMaybes ts)
            isPrimitive (EList { eContents = ts }) = all isPrimitive ts
            isPrimitive _ = False

        case parsed of
            [] -> raise' "no-expressions-parsed"
            is | all isPrimitive is -> evalAll is
            (i:_) -> return (Expression i)

    [$p|(p: Port) read-line|] =: do
        h <- getHandle [$e|p handle|]
        done <- liftIO (hIsEOF h)

        if done
            then raise' "end-of-input"
            else liftM String $ liftIO (TIO.hGetLine h)

    [$p|(p: Port) read-char|] =: do
        h <- getHandle [$e|p handle|]
        b <- liftIO (hGetBuffering h)
        liftIO (hSetBuffering h NoBuffering)
        c <- liftIO (hGetChar h)
        liftIO (hSetBuffering h b)

        return (Char c)

    [$p|(p: Port) contents|] =:
        getHandle [$e|p handle|] >>= liftM String . liftIO . TIO.hGetContents

    [$p|(p: Port) flush|] =:
        getHandle [$e|p handle|] >>= liftIO . hFlush
            >> return (particle "ok")

    [$p|(p: Port) close|] =:
        getHandle [$e|p handle|] >>= liftIO . hClose
            >> return (particle "ok")

    [$p|(p: Port) open?|] =:
        getHandle [$e|p handle|] >>= liftM Boolean . liftIO . hIsOpen

    [$p|(p: Port) closed?|] =:
        getHandle [$e|p handle|] >>= liftM Boolean . liftIO . hIsClosed

    [$p|(p: Port) readable?|] =:
        getHandle [$e|p handle|] >>= liftM Boolean . liftIO . hIsReadable

    [$p|(p: Port) writable?|] =:
        getHandle [$e|p handle|] >>= liftM Boolean . liftIO . hIsWritable

    [$p|(p: Port) seekable?|] =:
        getHandle [$e|p handle|] >>= liftM Boolean . liftIO . hIsSeekable

    [$p|(p: Port) ready?|] =:
        getHandle [$e|p handle|] >>= liftM Boolean . liftIO . hReady

    [$p|(p: Port) eof?|] =:
        getHandle [$e|p handle|] >>= liftM Boolean . liftIO . hIsEOF


    [$p|File new: (fn: String)|] =::: [$e|Port new: fn|]
    [$p|File open: (fn: String)|] =::: [$e|Port new: fn|]

    [$p|File read: (fn: String)|] =:::
        [$e|Port (new: fn &mode: @read) ensuring: @close do: @contents|]

    [$p|File delete: (fn: String)|] =: do
        fn <- getString [$e|fn|]
        checkExists fn
        liftIO (removeFile fn)
        return (particle "ok")

    [$p|File move: (from: String) to: (to: String)|] =:::
        [$e|File rename: from to: to|]
    [$p|File rename: (from: String) to: (to: String)|] =: do
        from <- getString [$e|from|]
        to <- getString [$e|to|]
        checkExists from
        liftIO (renameFile from to)
        return (particle "ok")

    [$p|File copy: (from: String) to: (to: String)|] =: do
        from <- getString [$e|from|]
        to <- getString [$e|to|]
        checkExists from
        liftIO (copyFile from to)
        return (particle "ok")

    [$p|File canonicalize-path: (fn: String)|] =: do
        fn <- getString [$e|fn|]
        liftM string $ liftIO (canonicalizePath fn)

    [$p|File make-relative: (fn: String)|] =: do
        fn <- getString [$e|fn|]
        liftM string $ liftIO (makeRelativeToCurrentDirectory fn)

    [$p|File exists?: (fn: String)|] =: do
        fn <- getString [$e|fn|]
        liftM Boolean $ liftIO (doesFileExist fn)

    [$p|File find-executable: (name: String)|] =: do
        name <- getString [$e|name|]
        find <- liftIO (findExecutable name)
        case find of
            Nothing -> return (particle "none")
            Just fn -> return (keyParticle ["ok"] [Nothing, Just (string fn)])

    [$p|File readable?: (fn: String)|] =: do
        fn <- getString [$e|fn|]
        checkExists fn
        liftM (Boolean . readable) $ liftIO (getPermissions fn)

    [$p|File writable?: (fn: String)|] =: do
        fn <- getString [$e|fn|]
        checkExists fn
        liftM (Boolean . writable) $ liftIO (getPermissions fn)

    [$p|File executable?: (fn: String)|] =: do
        fn <- getString [$e|fn|]
        checkExists fn
        liftM (Boolean . executable) $ liftIO (getPermissions fn)

    [$p|File searchable?: (fn: String)|] =: do
        fn <- getString [$e|fn|]
        checkExists fn
        liftM (Boolean . searchable) $ liftIO (getPermissions fn)

    [$p|File set-readable: (fn: String) to: (b: Boolean)|] =: do
        Boolean r <- here "b" >>= findBoolean
        fn <- getString [$e|fn|]
        checkExists fn
        ps <- liftIO (getPermissions fn)
        liftIO (setPermissions fn (ps { readable = r }))
        return (particle "ok")

    [$p|File set-writable: (fn: String) to: (b: Boolean)|] =: do
        Boolean w <- here "b" >>= findBoolean
        fn <- getString [$e|fn|]
        checkExists fn
        ps <- liftIO (getPermissions fn)
        liftIO (setPermissions fn (ps { writable = w }))
        return (particle "ok")

    [$p|File set-executable: (fn: String) to: (b: Boolean)|] =: do
        Boolean x <- here "b" >>= findBoolean
        fn <- getString [$e|fn|]
        checkExists fn
        ps <- liftIO (getPermissions fn)
        liftIO (setPermissions fn (ps { executable = x }))
        return (particle "ok")

    [$p|File set-searchable: (fn: String) to: (b: Boolean)|] =: do
        Boolean s <- here "b" >>= findBoolean
        fn <- getString [$e|fn|]
        checkExists fn
        ps <- liftIO (getPermissions fn)
        liftIO (setPermissions fn (ps { searchable = s }))
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
        checkDirExists path
        liftIO (removeDirectory path)
        return (particle "ok")

    [$p|Directory remove-recursive: (path: String)|] =: do
        path <- getString [$e|path|]
        checkDirExists path
        liftIO (removeDirectoryRecursive path)
        return (particle "ok")

    [$p|Directory move: (from: String) to: (to: String)|] =:::
        [$e|Directory rename: from to: to|]
    [$p|Directory rename: (from: String) to: (to: String)|] =: do
        from <- getString [$e|from|]
        to <- getString [$e|to|]
        checkDirExists from
        liftIO (renameDirectory from to)
        return (particle "ok")

    [$p|Directory contents: (path: String)|] =: do
        path <- getString [$e|path|]
        checkDirExists path
        liftM (list . map string . filter (`notElem` [".", ".."]))
            (liftIO (getDirectoryContents path))

    [$p|Directory current|] =:
        liftM string $ liftIO getCurrentDirectory

    [$p|Directory set-current: (path: String)|] =: do
        path <- getString [$e|path|]
        checkDirExists path
        liftIO (setCurrentDirectory path)
        return (particle "ok")

    [$p|Directory home|] =:
        liftM string $ liftIO getHomeDirectory

    [$p|Directory user-data-for: (app: String)|] =: do
        app <- getString [$e|app|]
        liftM string $ liftIO (getAppUserDataDirectory app)

    [$p|Directory user-documents|] =:
        liftM string $ liftIO getUserDocumentsDirectory

    [$p|Directory temporary|] =:
        liftM string $ liftIO getTemporaryDirectory

    [$p|Directory exists?: (path: String)|] =: do
        path <- getString [$e|path|]
        liftM Boolean $ liftIO (doesDirectoryExist path)

    [$p|(a: String) </> (b: String)|] =: do
        a <- getString [$e|a|]
        b <- getString [$e|b|]
        return (string (a </> b))

    [$p|(a: String) <.> (b: String)|] =: do
        a <- getString [$e|a|]
        b <- getString [$e|b|]
        return (string (a <.> b))

    [$p|interaction: (prompt: String)|] =: do
        prompt <- getString [$e|prompt|]
        history <- getString [$e|current-history-file|]
        line <-
            liftIO $ Haskeline.catch
                (liftM Just $ runInput history (getInputLine prompt))
                (\Interrupt -> return Nothing)

        case line of
            Just (Just i) -> return (string i)
            Just Nothing -> raise' "end-of-input"
            Nothing -> raise' "interrupt"

  where
    checkExists fn = do
        b <- liftIO (doesFileExist fn)

        if b
           then return ()
           else raise ["file-not-found"] [string fn]

    checkDirExists d = do
        b <- liftIO (doesDirectoryExist d)

        if b
           then return ()
           else raise ["directory-not-found"] [string d]

    runInput h
        = runInputT (defaultSettings { historyFile = Just h })
        . withInterrupt

    portObj hdl = newScope $ do
        port <- eval [$e|Port clone|]

        define (single "handle" (PMatch port))
            (EPrimitive Nothing $ haskell hdl)

        return port

    getHandle ex = eval ex >>= fromHaskell

    hGetSegment :: Handle -> IO String
    hGetSegment h = dropSpaces >> hGetSegment' Nothing
      where
        dropSpaces = do
            c <- hLookAhead h
            when (isSpace c) (hGetChar h >> dropSpaces)

        hGetSegment' stop = do
            end <- hIsEOF h

            if end
                then return ""
                else do

            c <- hGetChar h

            case c of
                '"' -> wrapped '"'
                '\'' -> wrapped '\''
                '(' -> nested '(' ')'
                '{' -> nested '{' '}'
                '[' -> nested '[' ']'
                s | (stop == Nothing && isSpace s) || (Just s == stop) ->
                    return [c]
                _ -> do
                    cs <- hGetSegment' stop
                    return (c:cs)
          where
            wrapped d = do
                w <- liftM (d:) $ hGetUntil h d
                rest <- hGetSegment' stop
                return (w ++ rest)

            nested c end = do
                sub <- liftM (c:) $ hGetSegment' (Just end)
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
