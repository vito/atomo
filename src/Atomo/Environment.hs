{-# LANGUAGE BangPatterns #-}
module Atomo.Environment where

import Control.Monad (filterM, forM, forM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Data.IORef
import Data.List (nub)
import Data.Maybe (isJust)
import System.Directory
import System.FilePath
import System.IO.Unsafe
import qualified Data.IntMap as M
import qualified Data.Vector as V
import qualified Language.Haskell.Interpreter as H
import qualified Text.PrettyPrint as P

import {-# SOURCE #-} Atomo.Method
import Atomo.Parser
import Atomo.Pretty
import Atomo.Types
import {-# SOURCE #-} qualified Atomo.Kernel as Kernel


-----------------------------------------------------------------------------
-- Execution ----------------------------------------------------------------
-----------------------------------------------------------------------------

exec :: VM () -> IO ()
exec x = execWith (initEnv >> x) startEnv

-- execute an action, cleanly printing an error log on error
execWith :: VM () -> Env -> IO ()
execWith x e = do
    haltChan <- newChan

    forkIO $ do
        runWith (go x >> lift (gets halt) >>= liftIO) e
            { halt = writeChan haltChan ()
            }

        return ()

    readChan haltChan

go :: VM () -> VM ()
go x = do
    res <- (fmap Right x) `catchError` (return . Left)
    case res of
        Left err -> printError err
        Right _ -> return ()

run :: VM a -> IO (Either AtomoError a)
run x = runWith (initEnv >> x) startEnv

runWith :: VM a -> Env -> IO (Either AtomoError a)
runWith x s = evalStateT (runErrorT x) s

printError :: AtomoError -> VM ()
printError err = do
    t <- traceback

    if not (null t)
        then do
            liftIO (putStrLn "traceback:")

            forM_ t $ \e -> liftIO $
                print (prettyStack e)
        else return ()

    liftIO (putStrLn "")
    liftIO . print . pretty $ err

    lift . modify $ \s -> s { stack = [] }
  where
    traceback = fmap (reverse . take 10 . reverse) (lift $ gets stack)

prettyVM :: Value -> VM P.Doc
prettyVM v@(List vr) = do
    vs <- fmap V.toList (liftIO (readIORef vr))
    if all isChar vs
        then return (pretty v)
        else do
    pvs <- mapM prettyVM vs
    return . P.brackets . P.hsep . P.punctuate P.comma $ pvs
prettyVM r@(Reference _) = do
    s <- dispatch (single "show" r) >>= liftIO . toString
    return (P.text s)
prettyVM v = return (pretty v)

-- | set up the primitive objects, etc.
initEnv :: VM ()
{-# INLINE initEnv #-}
initEnv = do
    -- the very root object
    object <- newObject id

    -- top scope is a proto delegating to the root object
    topObj <- newObject $ \o -> o { oDelegates = [object] }
    lift . modify $ \e -> e { top = topObj }

    -- define Object as the root object
    define (psingle "Object" PSelf) (Primitive Nothing object)
    lift . modify $ \e -> e { primitives = (primitives e) { idObject = rORef object } }

    -- this thread's channel
    chan <- liftIO newChan
    lift . modify $ \e -> e { channel = chan }

    -- define primitive objects
    forM_ primObjs $ \(n, f) -> do
        o <- newObject $ \o -> o { oDelegates = [object] }
        define (psingle n PSelf) (Primitive Nothing o)
        lift . modify $ \e -> e { primitives = f (primitives e) (rORef o) }

    listObj <- lift $ gets (idList . primitives)
    define (psingle "String" PSelf) (Primitive Nothing (Reference listObj))

    Kernel.load
  where
    primObjs =
        [ ("Block", \is r -> is { idBlock = r })
        , ("Char", \is r -> is { idChar = r })
        , ("Double", \is r -> is { idDouble = r })
        , ("Expression", \is r -> is { idExpression = r })
        , ("Integer", \is r -> is { idInteger = r })
        , ("List", \is r -> is { idList = r })
        , ("Message", \is r -> is { idMessage = r })
        , ("Particle", \is r -> is { idParticle = r })
        , ("Process", \is r -> is { idProcess = r })
        , ("Pattern", \is r -> is { idPattern = r })
        ]



-----------------------------------------------------------------------------
-- General ------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | evaluation
eval :: Expr -> VM Value
eval e = eval' e `catchError` pushStack
  where
    pushStack err = do
        lift . modify $ \s -> s { stack = e : stack s }
        throwError err

    eval' (Define { ePattern = p, eExpr = ev }) = do
        define p ev
        return (particle "ok")
    eval' (Set { ePattern = p@(PSingle {}), eExpr = ev }) = do
        v <- eval ev
        define p (Primitive (eLocation ev) v)
        return v
    eval' (Set { ePattern = p@(PKeyword {}), eExpr = ev }) = do
        v <- eval ev
        define p (Primitive (eLocation ev) v)
        return v
    eval' (Set { ePattern = p, eExpr = ev }) = do
        v <- eval ev

        is <- lift $ gets primitives
        if match is p v
            then do
                forM_ (bindings' p v) $ \(p', v') -> do
                    define p' (Primitive (eLocation ev) v')

                return v
            else throwError (Mismatch p v)
    eval' (Dispatch { eMessage = ESingle { emID = i, emName = n, emTarget = t } }) = do
        v <- eval t
        dispatch (Single i n v)
    eval' (Dispatch { eMessage = EKeyword { emID = i, emNames = ns, emTargets = ts } }) = do
        vs <- mapM eval ts
        dispatch (Keyword i ns vs)
    eval' (Operator { eNames = ns, eAssoc = a, ePrec = p }) = do
        forM_ ns $ \n -> lift . modify $ \s ->
            s { parserState = (n, (a, p)) : parserState s }

        return (particle "ok")
    eval' (Primitive { eValue = v }) = return v
    eval' (EBlock { eArguments = as, eContents = es }) = do
        t <- lift (gets top)
        return (Block t as es)
    eval' (EDispatchObject {}) = do
        c <- lift $ gets call
        newObject $ \o -> o
            { oMethods =
                ( toMethods
                    [ (psingle "sender" PSelf, callSender c)
                    , (psingle "message" PSelf, Message (callMessage c))
                    , (psingle "context" PSelf, callContext c)
                    ]
                , M.empty
                )
            }
    eval' (EList { eContents = es }) = do
        vs <- mapM eval es
        list vs
    eval' (EParticle { eParticle = EPMSingle n }) = return (Particle $ PMSingle n)
    eval' (EParticle { eParticle = EPMKeyword ns mes }) = do
        mvs <- forM mes $
            maybe (return Nothing) (fmap Just . eval)
        return (Particle $ PMKeyword ns mvs)
    eval' (ETop {}) = lift (gets top)
    eval' (EVM { eAction = x }) = x

-- | evaluating multiple expressions, returning the last result
evalAll :: [Expr] -> VM Value
evalAll [] = throwError . ErrorMsg $ "cannot evaluate 0 expressions" -- TODO: proper error
evalAll [e] = eval e
evalAll (e:es) = eval e >> evalAll es

-- | object creation
newObject :: (Object -> Object) -> VM Value
newObject f = fmap Reference . liftIO $
    newIORef . f $ Object
        { oDelegates = []
        , oMethods = (M.empty, M.empty)
        }

-- run x with t as its toplevel object
-- TODO: rebuild error stack
withTop :: Value -> VM a -> VM a
withTop t x = do
    e <- lift get
    Right res <- liftIO (runWith action (e { top = t }))
    either mergeStack return res
  where
    action = do
        res <- (fmap Right x) `catchError` (return . Left)
        case res of
            Left e -> do
                s <- lift $ gets stack
                return (Left (s, e))
            Right r -> return (Right r)

    mergeStack (st, e) = do
        lift . modify $ \s -> s { stack = st }
        throwError e



-----------------------------------------------------------------------------
-- Define -------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | define a pattern to evaluate an expression
define :: Pattern -> Expr -> VM ()
define !p !e = do
    is <- lift $ gets primitives
    newp <- methodPattern p
    os <- targets is newp
    m <- method newp e
    forM_ os $ \o -> do
        obj <- liftIO (readIORef o)

        let (oss, oks) = oMethods obj
            ms =
                case newp of
                    PSingle {} -> (addMethod (m o) oss, oks)
                    PKeyword {} -> (oss, addMethod (m o) oks)
                    _ -> error $ "impossible: defining with pattern " ++ show newp

        liftIO . writeIORef o $
            obj { oMethods = ms }
  where
    method p' (Primitive _ v) = return (\o -> Slot (setSelf o p') v)
    method p' e' = lift (gets top) >>= \t -> return (\o -> Method (setSelf o p') t e')

    methodPattern p'@(PSingle { ppTarget = t }) = do
        t' <- methodPattern t
        return p' { ppTarget = t' }
    methodPattern p'@(PKeyword { ppTargets = ts }) = do
        ts' <- mapM methodPattern ts
        return p' { ppTargets = ts' }
    methodPattern (PObject oe) = do
        v <- eval oe
        return (PMatch v)
    methodPattern (PNamed n p') = do
        p'' <- methodPattern p'
        return (PNamed n p'')
    methodPattern p' = return p'

    -- | Swap out a reference match with PSelf, for inserting on the object
    setSelf :: ORef -> Pattern -> Pattern
    setSelf o (PKeyword i ns ps) =
        PKeyword i ns (map (setSelf o) ps)
    setSelf o (PMatch (Reference x))
        | o == x = PSelf
    setSelf o (PNamed n p') =
        PNamed n (setSelf o p')
    setSelf o (PSingle i n t) =
        PSingle i n (setSelf o t)
    setSelf _ p' = p'



targets :: IDs -> Pattern -> VM [ORef]
targets _ (PMatch v) = orefFor v >>= return . (: [])
targets is (PSingle _ _ p) = targets is p
targets is (PKeyword _ _ ps) = do
    ts <- mapM (targets is) ps
    return (nub (concat ts))
targets is (PNamed _ p) = targets is p
targets _ PSelf = lift (gets top) >>= orefFor >>= return . (: [])
targets is PAny = return [idObject is]
targets is (PList _) = return [idList is]
targets is (PHeadTail _ _) = return [idList is]
targets _ p = error $ "no targets for " ++ show p



-----------------------------------------------------------------------------
-- Dispatch -----------------------------------------------------------------
-----------------------------------------------------------------------------

-- | dispatch a message and return a value
dispatch :: Message -> VM Value
dispatch !m = do
    find <- findFirstMethod m vs
    case find of
        Just method -> runMethod method m
        Nothing ->
            case vs of
                [v] -> sendDNU v
                _ -> sendDNUs vs 0
  where
    vs =
        case m of
            Single { mTarget = t } -> [t]
            Keyword { mTargets = ts } -> ts

    sendDNU v = do
        find <- findMethod v (dnuSingle v)
        case find of
            Nothing -> throwError $ DidNotUnderstand m
            Just method -> runMethod method (dnuSingle v)

    sendDNUs [] _ = throwError $ DidNotUnderstand m
    sendDNUs (v:vs') n = do
        find <- findMethod v (dnu v n)
        case find of
            Nothing -> sendDNUs vs' (n + 1)
            Just method -> runMethod method (dnu v n)

    dnu v n = keyword
        ["did-not-understand", "at"]
        [v, Message m, Integer n]

    dnuSingle v = keyword
        ["did-not-understand"]
        [v, Message m]


-- | find a method on object `o' that responds to `m', searching its
-- delegates if necessary
findMethod :: Value -> Message -> VM (Maybe Method)
findMethod v m = do
    is <- lift $ gets primitives
    r <- orefFor v
    o <- liftIO (readIORef r)
    case relevant (is { idMatch = r }) o m of
        Nothing -> findFirstMethod m (oDelegates o)
        Just mt -> return (Just mt)
    
-- | find the first value that has a method defiend for `m'
findFirstMethod :: Message -> [Value] -> VM (Maybe Method)
findFirstMethod _ [] = return Nothing
findFirstMethod m (v:vs) = do
    findMethod v m
        >>= maybe (findFirstMethod m vs) (return . Just)

-- | find a relevant method for message `m' on object `o'
relevant :: IDs -> Object -> Message -> Maybe Method
relevant ids o m =
    M.lookup (mID m) (methods m) >>= firstMatch ids m
  where
    methods (Single {}) = fst (oMethods o)
    methods (Keyword {}) = snd (oMethods o)

    firstMatch _ _ [] = Nothing
    firstMatch ids' m' (mt:mts)
        | match ids' (mPattern mt) (Message m') = Just mt
        | otherwise = firstMatch ids' m' mts

-- | check if a value matches a given pattern
-- note that this is much faster when pure, so it uses unsafePerformIO
-- to check things like delegation matches.
match :: IDs -> Pattern -> Value -> Bool
{-# NOINLINE match #-}
match ids PSelf (Reference y) =
    refMatch ids (idMatch ids) y
match ids PSelf y =
    match ids (PMatch (Reference (idMatch ids))) (Reference (orefFrom ids y))
match ids (PMatch (Reference x)) (Reference y) =
    refMatch ids x y
match ids (PMatch (Reference x)) y =
    match ids (PMatch (Reference x)) (Reference (orefFrom ids y))
match _ (PMatch x) y =
    x == y
match ids
    (PSingle { ppTarget = p })
    (Message (Single { mTarget = t })) =
    match ids p t
match ids
    (PKeyword { ppTargets = ps })
    (Message (Keyword { mTargets = ts })) =
    matchAll ids ps ts
match ids (PNamed _ p) v = match ids p v
match _ PAny _ = True
match ids (PList ps) (List v) = matchAll ids ps vs
  where
    vs = V.toList $ unsafePerformIO (readIORef v)
match ids (PHeadTail hp tp) (List v) =
    V.length vs > 0 && match ids hp h && match ids tp t
  where
    vs = unsafePerformIO (readIORef v)
    h = V.head vs
    t = List (unsafePerformIO (newIORef (V.tail vs)))
match _ (PPMSingle a) (Particle (PMSingle b)) = a == b
match ids (PPMKeyword ans aps) (Particle (PMKeyword bns mvs)) =
    ans == bns && matchParticle ids aps mvs
match _ _ _ = False

refMatch :: IDs -> ORef -> ORef -> Bool
refMatch ids x y = x == y || delegatesMatch
  where
    delegatesMatch = any
        (match ids (PMatch (Reference x)))
        (oDelegates (unsafePerformIO (readIORef y)))

-- | match multiple patterns with multiple values
matchAll :: IDs -> [Pattern] -> [Value] -> Bool
matchAll _ [] [] = True
matchAll ids (p:ps) (v:vs) = match ids p v && matchAll ids ps vs
matchAll _ _ _ = False

matchParticle :: IDs -> [Pattern] -> [Maybe Value] -> Bool
matchParticle _ [] [] = True
matchParticle ids (PAny:ps) (Nothing:mvs) = matchParticle ids ps mvs
matchParticle ids (PNamed _ p:ps) mvs = matchParticle ids (p:ps) mvs
matchParticle ids (p:ps) (Just v:mvs) = match ids p v && matchParticle ids ps mvs
matchParticle _ _ _ = False

-- evaluate a method in a scope with the pattern's bindings,
-- delegating to the method's context and setting the "dispatch" object
runMethod :: Method -> Message -> VM Value
runMethod (Slot { mValue = v }) _ = return v
runMethod (Method { mPattern = p, mTop = t, mExpr = e }) m = do
    nt <- newObject $ \o -> o
        { oDelegates = [t]
        , oMethods = (bindings p m, M.empty)
        }

    lift . modify $ \e' -> e'
        { call = Call
            { callSender = top e'
            , callMessage = m
            , callContext = t
            }
        }

    withTop nt $ eval e

-- evaluate an action in a new scope
newScope :: VM a -> VM a
newScope x = do
    t <- lift $ gets top
    nt <- newObject $ \o -> o
        { oDelegates = [t]
        }

    withTop nt x

-- | given a pattern and a message, return the bindings from the pattern
bindings :: Pattern -> Message -> MethodMap
bindings (PSingle { ppTarget = p }) (Single { mTarget = t }) =
    toMethods (bindings' p t)
bindings (PKeyword { ppTargets = ps }) (Keyword { mTargets = ts }) =
    toMethods $ concat (zipWith bindings' ps ts)
bindings p m = error $ "impossible: bindings on " ++ show (p, m)

bindings' :: Pattern -> Value -> [(Pattern, Value)]
bindings' (PNamed n p) v = (psingle n PSelf, v) : bindings' p v
bindings' (PPMKeyword _ ps) (Particle (PMKeyword _ mvs)) = concat
    $ map (\(p, Just v) -> bindings' p v)
    $ filter (isJust . snd)
    $ zip ps mvs
bindings' (PList ps) (List v) = concat (zipWith bindings' ps vs)
  where
    vs = V.toList $ unsafePerformIO (readIORef v)
bindings' (PHeadTail hp tp) (List v) =
    bindings' hp h ++ bindings' tp t
  where
    vs = unsafePerformIO (readIORef v)
    h = V.head vs
    t = List (unsafePerformIO (newIORef (V.tail vs)))
bindings' _ _ = []



-----------------------------------------------------------------------------
-- Helpers ------------------------------------------------------------------
-----------------------------------------------------------------------------

infixr 0 =:, =::

(=:) :: Pattern -> VM Value -> VM ()
pat =: vm = define pat (EVM Nothing vm)

(=::) :: Pattern -> Value -> VM ()
pat =:: v = define pat (Primitive Nothing v)

(=:::) :: Pattern -> Expr -> VM ()
pat =::: e = define pat e

findValue :: (Value -> Bool) -> Value -> VM Value
findValue t v | t v = return v
findValue t v = findValue' t v >>= maybe (error "could not find a value satisfying the predecate") return

findValue' :: (Value -> Bool) -> Value -> VM (Maybe Value)
findValue' t v | t v = return (Just v)
findValue' t (Reference r) = do
    o <- liftIO (readIORef r)
    findDels (oDelegates o)
  where
    findDels [] = return Nothing
    findDels (d:ds) = do
        f <- findValue' t d
        case f of
            Nothing -> findDels ds
            Just v -> return (Just v)
findValue' _ _ = return Nothing

getList :: Expr -> VM (V.Vector Value)
getList e = eval e >>= findValue isList >>= \(List v) -> liftIO . readIORef $ v

here :: String -> VM Value
here n =
    lift (gets top)
        >>= dispatch . (single n)

bool :: Bool -> VM Value
{-# INLINE bool #-}
bool True = here "True"
bool False = here "False"

referenceTo :: Value -> VM Value
{-# INLINE referenceTo #-}
referenceTo = fmap Reference . orefFor

doBlock :: MethodMap -> Value -> [Expr] -> VM Value
{-# INLINE doBlock #-}
doBlock bms s es = do
    blockScope <- newObject $ \o -> o
        { oDelegates = [s]
        , oMethods = (bms, M.empty)
        }

    withTop blockScope (evalAll es)

objectFor :: Value -> VM Object
{-# INLINE objectFor #-}
objectFor v = orefFor v >>= liftIO . readIORef

orefFor :: Value -> VM ORef
{-# INLINE orefFor #-}
orefFor v = lift (gets primitives) >>= \is -> return $ orefFrom is v

orefFrom :: IDs -> Value -> ORef
{-# INLINE orefFrom #-}
orefFrom _ (Reference r) = r
orefFrom ids (Block _ _ _) = idBlock ids
orefFrom ids (Char _) = idChar ids
orefFrom ids (Double _) = idDouble ids
orefFrom ids (Expression _) = idExpression ids
orefFrom ids (Integer _) = idInteger ids
orefFrom ids (List _) = idList ids
orefFrom ids (Message _) = idMessage ids
orefFrom ids (Particle _) = idParticle ids
orefFrom ids (Process _ _) = idProcess ids
orefFrom ids (Pattern _) = idPattern ids
orefFrom _ v = error $ "no orefFrom for: " ++ show v

-- load a file, either .atomo or .hs
loadFile :: FilePath -> VM ()
loadFile filename = do
    initialPath <- lift $ gets loadPath
    file <- findFile ("":initialPath)

    alreadyLoaded <- lift $ gets ((file `elem`) . loaded)
    if alreadyLoaded
        then return ()
        else do

    case takeExtension file of
        ".hs" -> do
            int <- liftIO . H.runInterpreter $ do
                H.loadModules [filename]
                H.setTopLevelModules ["Main"]
                H.interpret "load" (H.as :: VM ())

            load <- either (throwError . ImportError) return int

            load

        _ -> do
            source <- liftIO (readFile file)
            ast <- continuedParse source file

            lift . modify $ \s -> s { loadPath = [path] }

            mapM_ eval ast

            lift . modify $ \s -> s
                { loadPath = initialPath
                , loaded = file : loaded s
                }

  where
    path = takeDirectory (normalise filename)

    findFile [] = throwError (ErrorMsg ("file not found: " ++ filename)) -- TODO: proper error
    findFile (p:ps) = do
        check <- filterM (liftIO . doesFileExist . ((p </> filename) <.>)) exts

        case check of
            [] -> findFile ps
            (ext:_) -> liftIO (canonicalizePath $ p </> filename <.> ext)

    exts = ["", "atomo", "hs"]

-- | does one value delegate to another?
delegatesTo :: Value -> Value -> VM Bool
delegatesTo f t = do
    o <- objectFor f
    delegatesTo' (oDelegates o)
  where
    delegatesTo' [] = return False
    delegatesTo' (d:ds)
        | t `elem` (d:ds) = return True
        | otherwise = do
            o <- objectFor d
            delegatesTo' (oDelegates o ++ ds)

isA :: Value -> Value -> VM Bool
isA x y = do
    xr <- orefFor x
    yr <- orefFor y

    if xr == yr
        then return True
        else do
            ds <- fmap oDelegates (objectFor x)
            isA' ds
  where
    isA' [] = return False
    isA' (d:ds) = do
        di <- isA d y
        if di
            then return True
            else isA' ds
