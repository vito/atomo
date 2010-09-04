{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel (load) where

import Data.Dynamic
import Data.Hashable (hash)
import Data.IORef
import Data.Maybe (isJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO
import qualified Data.IntMap as M
import qualified Data.Vector as V

import Atomo.Debug
import Atomo.Environment
import Atomo.Haskell
import Atomo.Method
import Atomo.Pretty

load :: VM ()
load = do
    [$p|this|] =: eval [$e|dispatch sender|]

    [$p|(x: Object) clone|] =: do
        x <- here "x"
        newObject $ \o -> o
            { oDelegates = [x]
            }

    [$p|(x: Object) delegates-to: (y: Object)|] =: do
        f <- here "x" >>= orefFor
        t <- here "y"

        from <- liftIO (readIORef f)
        liftIO $ writeIORef f (from { oDelegates = oDelegates from ++ [t] })
        return (particle "ok")

    [$p|(x: Object) is-a?: (y: Object)|] =: do
        x <- here "x"
        y <- here "y"
        delegatesTo x y >>= bool

    [$p|(x: Object) responds-to?: (p: Particle)|] =: do
        x <- here "x"
        Particle p <- here "p" >>= findValue isParticle

        let completed =
                case p of
                    PMSingle n -> Single (hash n) n x
                    PMKeyword ns mvs -> Keyword (hash ns) ns (completeKP mvs [x])

        is <- gets ids

        objectFor x
            >>= findMethod is completed
            >>= bool . isJust

    [$p|(s: String) as: String|] =: do
        s <- here "s"
        cs <- fmap V.toList (getList [$e|s|])
        if all isChar cs
            then return s
            else string . show . pretty $ s

    [$p|(x: Object) as: String|] =:
        eval [$e|x show|]

    [$p|(x: Object) show|] =:
        here "x"
            >>= string . show . pretty

    [$p|(x: Object) dump|] =: do
        x <- here "x"
        liftIO (putStrLn (prettyShow x))
        return x

    [$p|load: (fn: String)|] =: do
        fn <- fmap V.toList $ getList [$e|fn|]

        if all isChar fn
            then loadFile (map (\(Char c) -> c) fn)
            else throwError $ ErrorMsg "@load: applied to non-String"

        return (particle "ok")

    [$p|v do: (b: Block)|] =: do
        v <- here "v"
        b <- here "b" >>= findValue isBlock
        joinWith v b []
        return v
    [$p|v do: (b: Block) with: (l: List)|] =: do
        v <- here "v"
        b <- here "b" >>= findValue isBlock
        as <- fmap V.toList $ getList [$e|l|]
        joinWith v b []
        return v

    [$p|v join: (b: Block)|] =: do
        v <- here "v"
        b <- here "b" >>= findValue isBlock
        joinWith v b []
    [$p|v join: (b: Block) with: (l: List)|] =: do
        v <- here "v"
        b <- here "b" >>= findValue isBlock
        as <- fmap V.toList $ getList [$e|l|]
        joinWith v b as


    loadNumeric
    loadList
    loadBlock
    loadExpression
    loadConcurrency
    loadMessage
    loadComparable
    loadParticle
    loadPorts
  where
    joinWith top (Block s ps es) as
        | length ps > length as = throwError . ErrorMsg . unwords $
            [ "block expects"
            , show (length ps)
            , "arguments, given"
            , show (length as)
            ]
        | null as = do
            case top of
                Reference r -> do
                    Object ds ms <- objectFor top
                    blockScope <- newObject $ \o -> o
                        { oDelegates = s:ds
                        , oMethods = ms
                        }

                    res <- withTop blockScope (evalAll es)
                    new <- objectFor blockScope
                    liftIO $ writeIORef r new
                        { oDelegates = tail (oDelegates new)
                        , oMethods = oMethods new
                        }

                    return res
                _ -> do
                    blockScope <- newObject $ \o -> o
                        { oDelegates = [top, s]
                        }

                    withTop blockScope (evalAll es)
        | otherwise = do
            -- a toplevel scope with transient definitions
            pseudoScope <- newObject $ \o -> o
                { oMethods = (bs, M.empty)
                }

            case top of
                Reference r -> do
                    Object ds ms <- objectFor top
                    -- the original prototype, but without its delegations
                    -- this is to prevent dispatch loops
                    doppelganger <- newObject $ \o -> o
                        { oMethods = ms
                        }

                    -- the main scope, methods are taken from here and merged with
                    -- the originals. delegates to the pseudoscope and doppelganger
                    -- so it has their methods in scope, but definitions go here
                    blockScope <- newObject $ \o -> o
                        { oDelegates = (pseudoScope:doppelganger:s:ds)
                        }

                    res <- withTop blockScope (evalAll es)
                    new <- objectFor blockScope
                    liftIO (writeIORef r new
                        { oDelegates = drop 3 (oDelegates new)
                        , oMethods = merge ms (oMethods new)
                        })

                    return res
                _ -> do
                    blockScope <- newObject $ \o -> o
                        { oDelegates = [top, pseudoScope, s]
                        }

                    withTop blockScope (evalAll es)
      where
        bs = addMethod (Slot (PSingle (hash "this") "this" PSelf) top) $
                toMethods . concat $ zipWith bindings' ps as

        merge (os, ok) (ns, nk) =
            ( foldl (flip addMethod) os (concat $ M.elems ns)
            , foldl (flip addMethod) ok (concat $ M.elems nk)
            )

loadBlock :: VM ()
loadBlock = do
    [$p|(b: Block) new: (l: List)|] =: do
        es <- fmap V.toList $ getList [$e|l|]
        s <- gets top

        let toExpr (Expression e) = e
            toExpr v = Primitive Nothing v

        return (Expression $ EBlock Nothing [] (map toExpr es))

    [$p|(b: Block) call|] =: do
        Block s as es <- here "b" >>= findValue isBlock

        if length as > 0
            then throwError . ErrorMsg $ "block expects " ++ show (length as) ++ ", given 0"
            else doBlock M.empty s es

    [$p|(b: Block) call: (l: List)|] =: do
        Block s ps es <- here "b" >>= findValue isBlock
        vs <- fmap V.toList $ getList [$e|l|]

        if length ps > length vs
            then throwError . ErrorMsg . unwords $
                [ "block expects"
                , show (length ps)
                , "arguments, given"
                , show (length vs)
                ]
            else doBlock (toMethods . concat $ zipWith bindings' ps vs) s es

    [$p|(b: Block) scope|] =: do
        Block s _ _ <- here "b" >>= findValue isBlock
        return s

    [$p|(b: Block) arguments|] =: do
        Block _ as _ <- here "b" >>= findValue isBlock
        list (map Pattern as)

    [$p|(b: Block) contents|] =: do
        Block _ _ es <- here "b" >>= findValue isBlock
        list (map Expression es)

    -- TODO: this probably shouldn't be in the kernel, but
    -- there aren't any timing primitives yet
    [$p|(b: Block) time|] =: do
        Block s _ es <- here "b" >>= findValue isBlock
        before <- liftIO getPOSIXTime
        doBlock M.empty s es
        after <- liftIO getPOSIXTime
        return $ Double (fromRational . toRational $ after - before)
    
loadExpression :: VM ()
loadExpression = do
    [$p|(e: Expression) evaluate|] =: do
        Expression e <- here "e" >>= findValue isExpression
        eval e

    [$p|(e: Expression) type|] =: do
        Expression e <- here "e" >>= findValue isExpression
        case e of
            Dispatch {} -> return (particle "dispatch")
            Define {} -> return (particle "define")
            Set {} -> return (particle "set")
            Primitive {} -> return (particle "primitive")
            EBlock {} -> return (particle "block")
            EDispatchObject {} -> return (particle "call")
            EVM {} -> return (particle "vm")
            EList {} -> return (particle "list")
            ETop {} -> return (particle "top")
            EParticle {} -> return (particle "particle")

    [$p|(e: Expression) dispatch-type|] =: do
        Expression (Dispatch _ d) <- here "e" >>= findValue isExpression
        case d of
            ESingle {} -> return (particle "single")
            EKeyword {} -> return (particle "keyword")

    [$p|(e: Expression) target|] =: do
        Expression (Dispatch _ (ESingle { emTarget = t })) <- here "e" >>= findValue isExpression
        return (Expression t)

    [$p|(e: Expression) particle|] =: do
        Expression (Dispatch _ em) <- here "e" >>= findValue isExpression

        case em of
            EKeyword { emNames = ns } ->
                return (keyParticle ns (replicate (length ns + 1) Nothing))
            ESingle { emName = n } -> return (particle n)

    [$p|(e: Expression) targets|] =: do
        Expression (Dispatch _ (EKeyword { emTargets = vs })) <- here "e" >>= findValue isExpression
        list (map Expression vs)

    [$p|(e: Expression) contents|] =: do
        Expression (EList _ es) <- here "e" >>= findValue isExpression
        list (map Expression es)

    [$p|(e: Expression) pattern|] =: do
        Expression e <- here "e" >>= findValue isExpression
        case e of
            Set { ePattern = p } -> return (Pattern p)
            Define { ePattern = p } -> return (Pattern p)

    [$p|(e: Expression) expression|] =: do
        Expression e <- here "e" >>= findValue isExpression
        case e of
            Set { eExpr = e } -> return (Expression e)
            Define { eExpr = e } -> return (Expression e)

loadConcurrency :: VM ()
loadConcurrency = do
    [$p|self|] =: do
        chan <- lift (gets channel)
        tid <- liftIO myThreadId
        return (Process chan tid)

    [$p|receive|] =: do
        chan <- lift (gets channel)
        v <- liftIO (readChan chan)
        return v

    [$p|halt|] =: gets halt >>= liftIO >> return (particle "ok")

    [$p|(p: Process) ! v|] =: do
        Process chan _ <- here "p" >>= findValue isProcess
        v <- here "v"
        liftIO (writeChan chan v)
        here "p"

    [$p|(b: Block) spawn|] =: do
        Block s as es <- here "b" >>= findValue isBlock

        if length as > 0
            then throwError . ErrorMsg $ "block expects " ++ show (length as) ++ ", given 0"
            else do
                st <- lift get
                chan <- liftIO newChan
                tid <- liftIO $ forkIO (runWith (doBlock M.empty s es) (st { channel = chan }) >> return ())
                return (Process chan tid)

    [$p|(b: Block) spawn: (l: List)|] =: do
        Block s as es <- here "b" >>= findValue isBlock
        vs <- fmap V.toList $ getList [$e|l|]

        if length as > length vs
            then throwError . ErrorMsg . unwords $
                [ "block expects"
                , show (length as)
                , "arguments, given"
                , show (length vs)
                ]
            else do
                st <- lift get
                chan <- liftIO newChan
                tid <- liftIO . forkIO $ do
                    runWith
                        (doBlock (toMethods . concat $ zipWith bindings' as vs) s es)
                        (st { channel = chan })

                    return ()
                return (Process chan tid)

    -- TODO: move this somewhere more sensible
    [$p|(n: Integer) sleep|] =: do
        Integer n <- here "n" >>= findValue isInteger
        liftIO (threadDelay (fromIntegral n))
        return (particle "ok")

    [$p|(p: Process) stop|] =: do
        Process _ tid <- here "p" >>= findValue isProcess
        liftIO (killThread tid)
        return (particle "ok")

loadNumeric :: VM ()
loadNumeric = do
    [$p|(a: Integer) sqrt|] =: do
        Integer a <- here "a" >>= findValue isInteger
        return (Double (sqrt (fromIntegral a)))

    [$p|(a: Double) sqrt|] =: do
        Double a <- here "a" >>= findValue isDouble
        return (Double (sqrt a))

    [$p|(a: Integer) + (b: Integer)|] =: primII (+)
    [$p|(a: Integer) + (b: Double)|] =: primID (+)
    [$p|(a: Double) + (b: Integer)|] =: primDI (+)
    [$p|(a: Double) + (b: Double)|] =: primDD (+)
    [$p|(a: Integer) - (b: Integer)|] =: primII (-)
    [$p|(a: Integer) - (b: Double)|] =: primID (-)
    [$p|(a: Double) - (b: Integer)|] =: primDI (-)
    [$p|(a: Double) - (b: Double)|] =: primDD (-)
    [$p|(a: Integer) * (b: Integer)|] =: primII (*)
    [$p|(a: Integer) * (b: Double)|] =: primID (*)
    [$p|(a: Double) * (b: Integer)|] =: primDI (*)
    [$p|(a: Double) * (b: Double)|] =: primDD (*)
    [$p|(a: Integer) / (b: Integer)|] =: primII div
    [$p|(a: Integer) / (b: Double)|] =: primID (/)
    [$p|(a: Double) / (b: Integer)|] =: primDI (/)
    [$p|(a: Double) / (b: Double)|] =: primDD (/)
    [$p|(a: Integer) ^ (b: Integer)|] =: primII (^)
    [$p|(a: Integer) ^ (b: Double)|] =: primID (**)
    [$p|(a: Double) ^ (b: Integer)|] =: primDI (**)
    [$p|(a: Double) ^ (b: Double)|] =: primDD (**)

    [$p|(a: Integer) % (b: Integer)|] =: primII mod
    [$p|(a: Integer) quotient: (b: Integer)|] =: primII quot
    [$p|(a: Integer) remainder: (b: Integer)|] =: primII rem
  where
    primII f = do
        Integer a <- here "a" >>= findValue isInteger
        Integer b <- here "b" >>= findValue isInteger
        return (Integer (f a b))

    primID f = do
        Integer a <- here "a" >>= findValue isInteger
        Double b <- here "b" >>= findValue isDouble
        return (Double (f (fromIntegral a) b))

    primDI f = do
        Double a <- here "a" >>= findValue isDouble
        Integer b <- here "b" >>= findValue isInteger
        return (Double (f a (fromIntegral b)))

    primDD f = do
        Double a <- here "a" >>= findValue isDouble
        Double b <- here "b" >>= findValue isDouble
        return (Double (f a b))

loadMessage :: VM ()
loadMessage = do
    [$p|(m: Message) type|] =: do
        Message m <- here "m" >>= findValue isMessage
        case m of
            Single {} -> return (particle "single")
            Keyword {} -> return (particle "keyword")

    [$p|(m: Message) particle|] =: do
        Message m <- here "m" >>= findValue isMessage
        case m of
            Single { mName = n } -> return (particle n)
            Keyword { mNames = ns } -> return (keyParticle ns (replicate (length ns + 1) Nothing))

    [$p|(m: Message) target|] =: do
        Message (Single { mTarget = t }) <- here "m" >>= findValue isMessage
        return t

    [$p|(m: Message) targets|] =: do
        Message (Keyword { mTargets = ts }) <- here "m" >>= findValue isMessage
        list ts

loadComparable :: VM ()
loadComparable = do
    [$p|a equals: b|] =: do
        a <- here "a"
        b <- here "b"
        bool (a == b)

    [$p|(a: Char) < (b: Char)|] =: do
        Char a <- here "a" >>= findValue isChar
        Char b <- here "b" >>= findValue isChar
        bool (a < b)

    [$p|(a: Integer) < (b: Integer)|] =: do
        Integer a <- here "a" >>= findValue isInteger
        Integer b <- here "b" >>= findValue isInteger
        bool (a < b)

    [$p|(a: Integer) < (b: Double)|] =: do
        Integer a <- here "a" >>= findValue isInteger
        Double b <- here "b" >>= findValue isDouble
        bool (fromIntegral a < b)

    [$p|(a: Double) < (b: Integer)|] =: do
        Double a <- here "a" >>= findValue isDouble
        Integer b <- here "b" >>= findValue isInteger
        bool (a < fromIntegral b)

    [$p|(a: Double) < (b: Double)|] =: do
        Double a <- here "a" >>= findValue isDouble
        Double b <- here "b" >>= findValue isDouble
        bool (a < b)

    [$p|(a: Char) > (b: Char)|] =: do
        Char a <- here "a" >>= findValue isChar
        Char b <- here "b" >>= findValue isChar
        bool (a > b)

    [$p|(a: Integer) > (b: Integer)|] =: do
        Integer a <- here "a" >>= findValue isInteger
        Integer b <- here "b" >>= findValue isInteger
        bool (a > b)

    [$p|(a: Integer) > (b: Double)|] =: do
        Integer a <- here "a" >>= findValue isInteger
        Double b <- here "b" >>= findValue isDouble
        bool (fromIntegral a > b)

    [$p|(a: Double) > (b: Integer)|] =: do
        Double a <- here "a" >>= findValue isDouble
        Integer b <- here "b" >>= findValue isInteger
        bool (a > fromIntegral b)

    [$p|(a: Double) > (b: Double)|] =: do
        Double a <- here "a" >>= findValue isDouble
        Double b <- here "b" >>= findValue isDouble
        bool (a > b)

    [$p|(a: Char) <= (b: Char)|] =: do
        Char a <- here "a" >>= findValue isChar
        Char b <- here "b" >>= findValue isChar
        bool (a <= b)

    [$p|(a: Integer) <= (b: Integer)|] =: do
        Integer a <- here "a" >>= findValue isInteger
        Integer b <- here "b" >>= findValue isInteger
        bool (a <= b)

    [$p|(a: Integer) <= (b: Double)|] =: do
        Integer a <- here "a" >>= findValue isInteger
        Double b <- here "b" >>= findValue isDouble
        bool (fromIntegral a <= b)

    [$p|(a: Double) <= (b: Integer)|] =: do
        Double a <- here "a" >>= findValue isDouble
        Integer b <- here "b" >>= findValue isInteger
        bool (a <= fromIntegral b)

    [$p|(a: Double) <= (b: Double)|] =: do
        Double a <- here "a" >>= findValue isDouble
        Double b <- here "b" >>= findValue isDouble
        bool (a <= b)

    [$p|(a: Char) >= (b: Char)|] =: do
        Char a <- here "a" >>= findValue isChar
        Char b <- here "b" >>= findValue isChar
        bool (a >= b)

    [$p|(a: Integer) >= (b: Integer)|] =: do
        Integer a <- here "a" >>= findValue isInteger
        Integer b <- here "b" >>= findValue isInteger
        bool (a >= b)

    [$p|(a: Integer) >= (b: Double)|] =: do
        Integer a <- here "a" >>= findValue isInteger
        Double b <- here "b" >>= findValue isDouble
        bool (fromIntegral a >= b)

    [$p|(a: Double) >= (b: Integer)|] =: do
        Double a <- here "a" >>= findValue isDouble
        Integer b <- here "b" >>= findValue isInteger
        bool (a >= fromIntegral b)

    [$p|(a: Double) >= (b: Double)|] =: do
        Double a <- here "a" >>= findValue isDouble
        Double b <- here "b" >>= findValue isDouble
        bool (a >= b)

    [$p|(a: Char) == (b: Char)|] =: do
        Char a <- here "a" >>= findValue isChar
        Char b <- here "b" >>= findValue isChar
        bool (a == b)

    [$p|(a: Integer) == (b: Integer)|] =: do
        Integer a <- here "a" >>= findValue isInteger
        Integer b <- here "b" >>= findValue isInteger
        bool (a == b)

    [$p|(a: Integer) == (b: Double)|] =: do
        Integer a <- here "a" >>= findValue isInteger
        Double b <- here "b" >>= findValue isDouble
        bool (fromIntegral a == b)

    [$p|(a: Double) == (b: Integer)|] =: do
        Double a <- here "a" >>= findValue isDouble
        Integer b <- here "b" >>= findValue isInteger
        bool (a == fromIntegral b)

    [$p|(a: Double) == (b: Double)|] =: do
        Double a <- here "a" >>= findValue isDouble
        Double b <- here "b" >>= findValue isDouble
        bool (a == b)

    [$p|(a: List) == (b: List)|] =: do
        as <- fmap V.toList $ getList [$e|a|]
        bs <- fmap V.toList $ getList [$e|b|]

        if length as == length bs
            then do
                eqs <- zipWithM (\a b -> dispatch (Keyword (hash ["=="]) ["=="] [a, b])) as bs
                true <- bool True
                bool (all (== true) eqs)
            else bool False

    [$p|(a: Process) == (b: Process)|] =: do
        Process _ a <- here "a" >>= findValue isProcess
        Process _ b <- here "b" >>= findValue isProcess
        bool (a == b)

    [$p|(a: Message) == (b: Message)|] =: do
        Message a <- here "a" >>= findValue isMessage
        Message b <- here "b" >>= findValue isMessage

        true <- bool True
        case (a, b) of
            (Single ai _ at, Single bi _ bt) -> do
                t <- dispatch (Keyword (hash ["=="]) ["=="] [at, bt])
                bool (ai == bi && t == true)
            (Keyword ai _ avs, Keyword bi _ bvs)
                | ai == bi && length avs == length bvs -> do
                eqs <- zipWithM (\x y -> dispatch (Keyword (hash ["=="]) ["=="] [x, y])) avs bvs
                bool (all (== true) eqs)
            _ -> bool False

    [$p|(a: Particle) == (b: Particle)|] =: do
        Particle a <- here "a" >>= findValue isParticle
        Particle b <- here "b" >>= findValue isParticle

        true <- bool True
        case (a, b) of
            (PMSingle an, PMSingle bn) ->
                bool (an == bn)
            (PMKeyword ans avs, PMKeyword bns bvs)
                | ans == bns && length avs == length bvs -> do
                eqs <- zipWithM (\mx my ->
                    case (mx, my) of
                        (Nothing, Nothing) -> return true
                        (Just x, Just y) ->
                            dispatch (Keyword (hash ["=="]) ["=="] [x, y])
                        _ -> bool False) avs bvs
                bool (all (== true) eqs)
            _ -> bool False

loadParticle :: VM ()
loadParticle = do
    [$p|(p: Particle) call: (l: List)|] =: do
        Particle p <- here "p" >>= findValue isParticle
        vs <- fmap V.toList $ getList [$e|l|]

        case p of
            PMKeyword ns mvs -> do
                let blanks = length (filter (== Nothing) mvs)

                if blanks > length vs
                    then throwError . ErrorMsg . unwords $
                            [ "particle needs"
                            , show blanks
                            , "values to complete,"
                            , show (length vs)
                            ]
                    else dispatch (Keyword (hash ns) ns $ completeKP mvs vs)
            PMSingle n -> do
                if length vs == 0
                    then throwError . ErrorMsg $ "particle needs 1 values to complete, given 0"
                    else dispatch (Single (hash n) n (head vs))

    [$p|(p: Particle) name|] =: do
        Particle (PMSingle n) <- here "p"
        list (map Char n)

    [$p|(p: Particle) names|] =: do
        Particle (PMKeyword ns _) <- here "p"
        mapM (list . map Char) ns >>= list

loadList :: VM ()
loadList = do
    [$p|(l: List) length|] =:
        getList [$e|l|] >>= return . Integer . fromIntegral . V.length

    [$p|(l: List) empty?|] =:
        getList [$e|l|] >>= bool . V.null

    [$p|(l: List) at: (n: Integer)|] =: do
        Integer n <- here "n" >>= findValue isInteger
        vs <- getList [$e|l|]
        return (vs V.! fromIntegral n)

    [$p|(l: List) head|] =:
        getList [$e|l|] >>= return . V.head

    [$p|(l: List) last|] =:
        getList [$e|l|] >>= return . V.last

    [$p|(l: List) from: (s: Integer) to: (e: Integer)|] =: do
        vs <- getList [$e|l|]
        Integer start <- here "s" >>= findValue isInteger
        Integer end <- here "e" >>= findValue isInteger
        list' (V.slice (fromIntegral start) (fromIntegral end) vs)

    [$p|(l: List) init|] =:
        getList [$e|l|] >>= list' . V.init

    [$p|(l: List) tail|] =:
        getList [$e|l|] >>= list' . V.tail

    [$p|(l: List) take: (n: Integer)|] =: do
        vs <- getList [$e|l|]
        Integer n <- here "n" >>= findValue isInteger
        list' (V.take (fromIntegral n) vs)

    [$p|(l: List) drop: (n: Integer)|] =: do
        vs <- getList [$e|l|]
        Integer n <- here "n" >>= findValue isInteger
        list' (V.drop (fromIntegral n) vs)

    [$p|v replicate: (n: Integer)|] =: do
        v <- here "v"
        Integer n <- here "n" >>= findValue isInteger
        list' (V.replicate (fromIntegral n) v)

    [$p|b repeat: (n: Integer)|] =: do
        b <- here "b"
        Integer n <- here "n" >>= findValue isInteger
        vs <- V.replicateM (fromIntegral n) $
            dispatch (Single (hash "call") "call" b)
        list' vs

    [$p|(a: List) .. (b: List)|] =: do
        as <- getList [$e|a|]
        bs <- getList [$e|b|]
        list' (as V.++ bs)

    [$p|(l: List) reverse|] =: do
        getList [$e|l|] >>= list' . V.reverse

    [$p|(l: List) map: b|] =: do
        vs <- getList [$e|l|]
        b <- here "b"

        nvs <- V.mapM (\v -> do
            as <- list' (V.singleton v)
            dispatch (Keyword (hash ["call"]) ["call"] [b, as])) vs

        list' nvs

    -- TODO: zip

    [$p|(x: List) zip: (y: List) with: b|] =: do
        xs <- getList [$e|x|]
        ys <- getList [$e|y|]
        b <- here "b"

        nvs <- V.zipWithM (\x y -> do
            as <- list' (V.fromList [x, y])
            dispatch (Keyword (hash ["call"]) ["call"] [b, as])) xs ys

        list' nvs

    [$p|(l: List) filter: b|] =: do
        vs <- getList [$e|l|]
        b <- here "b"

        t <- bool True
        nvs <- V.filterM (\v -> do
            as <- list' (V.singleton v)
            check <- dispatch (Keyword (hash ["call"]) ["call"] [b, as])
            return (check == t)) vs

        list' nvs

    [$p|(l: List) reduce: b|] =: do
        vs <- getList [$e|l|]
        b <- here "b"

        V.fold1M (\x acc -> do
            as <- list [x, acc]
            dispatch (Keyword (hash ["call"]) ["call"] [b, as])) vs

    [$p|(l: List) reduce: b with: v|] =: do
        vs <- getList [$e|l|]
        b <- here "b"
        v <- here "v"

        V.foldM (\x acc -> do
            as <- list [x, acc]
            dispatch (Keyword (hash ["call"]) ["call"] [b, as])) v vs

    [$p|(l: List) concat|] =: eval [$e|l reduce: @.. with: []|]
    [$p|(l: List) sum|] =: eval [$e|l reduce: @+ with: 0|]
    [$p|(l: List) product|] =: eval [$e|l reduce: @* with: 1|]
    [$p|(l: List) maximum|] =: eval [$e|l reduce: @max:|]
    [$p|(l: List) minimum|] =: eval [$e|l reduce: @min:|]

    [$p|(l: List) all?: b|] =: do
        vs <- getList [$e|l|]
        b <- here "b"

        t <- bool True
        nvs <- V.mapM (\v -> do
            as <- list' (V.singleton v)
            check <- dispatch (Keyword (hash ["call"]) ["call"] [b, as])
            return (check == t)) vs

        bool (V.and nvs)

    [$p|(l: List) any?: b|] =: do
        vs <- getList [$e|l|]
        b <- here "b"

        t <- bool True
        nvs <- V.mapM (\v -> do
            as <- list' (V.singleton v)
            check <- dispatch (Keyword (hash ["call"]) ["call"] [b, as])
            return (check == t)) vs

        bool (V.or nvs)

    [$p|(l: List) and|] =: do
        vs <- getList [$e|l|]
        t <- bool True
        bool (V.all (== t) vs)

    [$p|(l: List) or|] =: do
        vs <- getList [$e|l|]
        t <- bool True
        bool (V.any (== t) vs)

    -- TODO: take-while, drop-while

    [$p|(l: List) contains?: v|] =: do
        vs <- getList [$e|l|]
        v <- here "v"
        bool (v `V.elem` vs)

    -- TODO: find

    [$p|(x: Integer) .. (y: Integer)|] =: do
        Integer x <- here "x" >>= findValue isInteger
        Integer y <- here "y" >>= findValue isInteger

        if x < y
            then dispatch (Keyword (hash ["up-to"]) ["up-to"] [Integer x, Integer y])
            else dispatch (Keyword (hash ["down-to"]) ["down-to"] [Integer x, Integer y])

    [$p|(x: Integer) ... (y: Integer)|] =: do
        Integer x <- here "x" >>= findValue isInteger
        Integer y <- here "y" >>= findValue isInteger

        if x < y
            then dispatch (Keyword (hash ["up-to"]) ["up-to"] [Integer x, Integer (y - 1)])
            else dispatch (Keyword (hash ["down-to"]) ["down-to"] [Integer x, Integer (y + 1)])

    [$p|(x: Integer) to: (y: Integer) by: (d: Integer)|] =: do
        Integer x <- here "x" >>= findValue isInteger
        Integer y <- here "y" >>= findValue isInteger
        Integer d <- here "d" >>= findValue isInteger

        list' $ V.generate
            (fromIntegral $ abs ((y - x) `div` d) + 1)
            (Integer . (x +) . (* d) . fromIntegral)

    [$p|(x: Integer) up-to: (y: Integer)|] =: eval [$e|x to: y by: 1|]
    [$p|(x: Integer) down-to: (y: Integer)|] =: eval [$e|x to: y by: -1|]

    -- destructive update
    [$p|(l: List) at: (n: Integer) put: v|] =: do
        List l <- here "l" >>= findValue isList
        vs <- getList [$e|l|]

        Integer n <- here "n" >>= findValue isInteger
        v <- here "v"

        liftIO . writeIORef l $ vs V.// [(fromIntegral n, v)]

        return (List l)

    [$p|(l: List) << v|] =: eval [$e|l push: v|]
    [$p|(l: List) push: v|] =: do
        List l <- here "l" >>= findValue isList
        vs <- getList [$e|l|]
        v <- here "v"

        liftIO . writeIORef l $ V.snoc vs v

        return (List l)

loadPorts :: VM ()
loadPorts = do
    port <- eval [$e|Object clone|]
    [$p|Port|] =: return port

    [$p|Port standard-input|] =: portObj stdin
    [$p|Port standard-output|] =: portObj stdout
    [$p|Port standard-error|] =: portObj stderr

    [$p|current-output-port|] =: eval [$e|Port standard-output|]
    [$p|current-input-port|] =: eval [$e|Port standard-input|]

    [$p|Port new: (fn: String)|] =: eval [$e|Port new: fn mode: @read-write|]
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
        Haskell out <- eval [$e|dispatch sender current-output-port handle|]

        List str <- eval [$e|x as: String|]
        cs <- fmap V.toList (liftIO (readIORef str))

        let hdl = fromDyn out stdout

        if all isChar cs
            then do
                liftIO (hPutStrLn hdl (map (\(Char c) -> c) cs))
                liftIO (hFlush hdl)
                return x
            else throwError $ ErrorMsg "@as:String returned non-String"

    [$p|read-line|] =: do
        Haskell inh <- eval [$e|dispatch sender current-input-port handle|]
        line <- liftIO (hGetLine (fromDyn inh stdin))
        string line
  where
    portObj hdl = do
        port <- eval [$e|Port clone|]
        [$p|p|] =: return port
        [$p|p handle|] =: return (Haskell (toDyn hdl))
        here "p"

completeKP :: [Maybe Value] -> [Value] -> [Value]
completeKP [] [] = []
completeKP (Nothing:mvs') (v:vs') = v : completeKP mvs' vs'
completeKP (Just v:mvs') vs' = v : completeKP mvs' vs'
completeKP mvs' vs' = error $ "impossible: completeKP on " ++ show (mvs', vs')
