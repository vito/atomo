{-# LANGUAGE BangPatterns #-}
module Atomo.Environment where

import Control.Monad.Error
import Control.Monad.State
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Data.IORef
import Data.Hashable
import Data.List (nub)
import System.Directory
import System.FilePath
import System.IO.Unsafe
import qualified Data.IntMap as M
import qualified Data.Vector as V

import {-# SOURCE #-} Atomo.Method
import Atomo.Parser
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
        runWith go e
            { halt = writeChan haltChan ()
            }

        return ()

    readChan haltChan
  where
    go = do
        res <- (fmap Right x) `catchError` (return . Left)
        case res of
            Left err -> do
                s <- gets stack
                mapM_ (liftIO . print) (take 10 (reverse s))
                liftIO (print err)
            Right _ -> return ()

        gets halt >>= liftIO

run :: VM a -> IO (Either AtomoError a)
run x = runWith (initEnv >> x) startEnv

runWith :: VM a -> Env -> IO (Either AtomoError a)
runWith x s = evalStateT (runErrorT x) s

-- | set up the primitive objects, etc.
initEnv :: VM ()
{-# INLINE initEnv #-}
initEnv = do
    -- the very root object
    object <- newObject id

    -- top scope is a proto delegating to the root object
    topObj <- newObject $ \o -> o { oDelegates = [object] }
    modify $ \e -> e { top = topObj }

    -- define Object as the root object
    define (PSingle (hash "Object") "Object" (PMatch topObj)) (Primitive Nothing object)
    modify $ \e -> e { ids = (ids e) { idObject = rORef object } }

    -- this thread's channel
    chan <- liftIO newChan
    modify $ \e -> e { channel = chan }

    -- define primitive objects
    forM_ primitives $ \(n, f) -> do
        o <- newObject $ \o -> o { oDelegates = [object] }
        define (PSingle (hash n) n (PMatch topObj)) (Primitive Nothing o)
        modify $ \e -> e { ids = f (ids e) (rORef o) }

    list <- gets (idList . ids)
    define (PSingle (hash "String") "String" (PMatch topObj)) (Primitive Nothing (Reference list))

    Kernel.load
  where
    primitives =
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
        modify $ \s -> s { stack = e : stack s }
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

        is <- gets ids
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
    eval' (Primitive { eValue = v }) = return v
    eval' (EBlock { eArguments = as, eContents = es }) = do
        t <- gets top
        return (Block t as es)
    eval' (EDispatchObject {}) = do
        c <- gets call
        newObject $ \o -> o
            { oMethods =
                ( toMethods
                    [ (PSingle (hash "sender") "sender" PSelf, callSender c)
                    , (PSingle (hash "message") "message" PSelf, Message (callMessage c))
                    , (PSingle (hash "context") "context" PSelf, callContext c)
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
            maybe (return Nothing) (\e -> eval e >>= return . Just)
        return (Particle $ PMKeyword ns mvs)
    eval' (ETop {}) = gets top
    eval' (EVM { eAction = x }) = x
    eval' _ = error $ "no eval for " ++ show e

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
withTop :: Value -> VM a -> VM a
withTop t x = do
    e <- get
    res <- liftIO (runWith x (e { top = t }))
    either throwError return res



-----------------------------------------------------------------------------
-- Define -------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | define a pattern to evaluate an expression
define :: Pattern -> Expr -> VM ()
define !p !e = do
    is <- gets ids
    newp <- methodPattern p
    os <- targets is newp
    m <- method newp e
    forM_ os $ \o -> do
        obj <- liftIO (readIORef o)

        let (oss, oks) = oMethods obj
            ms =
                case newp of
                    PSingle {} -> (addMethod m oss, oks)
                    PKeyword {} -> (oss, addMethod m oks)

        liftIO . writeIORef o $
            obj { oMethods = ms }
  where
    method p (Primitive _ v) = return (Slot p v)
    method p e = gets top >>= \t -> return (Method p t e)

    methodPattern p'@(PSingle { ppTarget = t }) = do
        t' <- methodPattern t
        return p' { ppTarget = t' }
    methodPattern p'@(PKeyword { ppTargets = ts }) = do
        ts' <- mapM methodPattern ts
        return p' { ppTargets = ts' }
    methodPattern (PObject e) = do
        v <- eval e
        return (PMatch v)
    methodPattern (PNamed n p) = do
        p' <- methodPattern p
        return (PNamed n p')
    methodPattern p' = return p'


targets :: IDs -> Pattern -> VM [ORef]
targets is (PMatch v) = orefFor v >>= return . (: [])
targets is (PSingle _ _ p) = targets is p
targets is (PKeyword _ _ ps) = do
    ts <- mapM (targets is) ps
    return (nub (concat ts))
targets is (PNamed _ p) = targets is p
targets is PSelf = gets top >>= orefFor >>= return . (: [])
targets is PAny = return [idObject is]
targets is (PList _) = return [idList is]
targets _ p = error $ "no targets for " ++ show p



-----------------------------------------------------------------------------
-- Dispatch -----------------------------------------------------------------
-----------------------------------------------------------------------------

-- | dispatch a message and return a value
dispatch :: Message -> VM Value
dispatch !m = do
    find <- findFirstMethod m vs
    case find of
        {-Just (Slot _ v) -> return v-}
        Just method -> runMethod method m
        Nothing -> throwError $ DidNotUnderstand m
  where
    vs =
        case m of
            Single { mTarget = t } -> [t]
            Keyword { mTargets = ts } -> ts

-- | find a method on object `o' that responds to `m', searching its
-- delegates if necessary
findMethod :: IDs -> Message -> Object -> VM (Maybe Method)
findMethod is m o = do
    case relevant is o m of
        Nothing -> findFirstMethod m (oDelegates o)
        Just mt -> return (Just mt)
    
-- | find the first value that has a method defiend for `m'
findFirstMethod :: Message -> [Value] -> VM (Maybe Method)
findFirstMethod _ [] = return Nothing
findFirstMethod m (v:vs) = do
    r <- orefFor v
    is <- gets ids

    liftIO (readIORef r)
        >>= findMethod (is { idMatch = r }) m
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
match ids (PMatch (Reference x)) (Reference y) =
    x == y || delegatesMatch
  where
    delegatesMatch = any
        (match ids (PMatch (Reference x)))
        (oDelegates (unsafePerformIO (readIORef y)))
match ids (PMatch (Reference x)) y =
    match ids (PMatch (Reference x)) (Reference (orefFrom ids y))
match ids (PMatch x) y =
    x == y
match ids
    (PSingle { ppTarget = p })
    (Message (Single { mTarget = t })) =
    match ids p t
match ids
    (PKeyword { ppTargets = ps })
    (Message (Keyword { mTargets = ts })) =
    matchAll ids ps ts
match ids PSelf v =
    match ids (PMatch (Reference (idMatch ids))) v
match ids (PNamed _ p) v = match ids p v
match _ PAny _ = True
match ids (PList ps) (List v) = matchAll ids ps vs
  where
    vs = V.toList $ unsafePerformIO (readIORef v)
match _ _ _ = False

-- | match multiple patterns with multiple values
matchAll :: IDs -> [Pattern] -> [Value] -> Bool
matchAll _ [] [] = True
matchAll ids (p:ps) (v:vs) = match ids p v && matchAll ids ps vs
matchAll _ _ _ = False

-- evaluate a method in a scope with the pattern's bindings,
-- delegating to the method's context and setting the "dispatch" object
runMethod :: Method -> Message -> VM Value
runMethod (Slot { mValue = v }) _ = return v
runMethod (Method { mPattern = p, mTop = t, mExpr = e }) m = do
    nt <- newObject $ \o -> o
        { oDelegates = [t]
        , oMethods = (bindings p m, M.empty)
        }

    modify $ \e -> e
        { call = Call
            { callSender = top e
            , callMessage = m
            , callContext = t
            }
        }

    withTop nt $ eval e

-- | given a pattern and a message, return the bindings from the pattern
bindings :: Pattern -> Message -> MethodMap
bindings (PSingle { ppTarget = p }) (Single { mTarget = t }) =
    toMethods (bindings' p t)
bindings (PKeyword { ppTargets = ps }) (Keyword { mTargets = ts }) =
    toMethods $ concat (zipWith bindings' ps ts)

bindings' :: Pattern -> Value -> [(Pattern, Value)]
bindings' (PNamed n p) v = (PSingle (hash n) n PSelf, v) : bindings' p v
bindings' (PPMKeyword _ ps) (Particle (PMKeyword _ mvs)) = concat
    $ map (\(p, Just v) -> bindings' p v)
    $ filter (\(_, mv) -> case mv of { Nothing -> False; _ -> True })
    $ zip ps mvs
bindings' (PList ps) (List v) = concat (zipWith bindings' ps vs)
  where
    vs = V.toList $ unsafePerformIO (readIORef v)
bindings' _ _ = []



-----------------------------------------------------------------------------
-- Helpers ------------------------------------------------------------------
-----------------------------------------------------------------------------

infixr 0 =:, =::

(=:) :: Pattern -> VM Value -> VM ()
pat =: vm = define pat (EVM Nothing vm)

(=::) :: Pattern -> Value -> VM ()
pat =:: vm = define pat (Primitive Nothing vm)

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
    gets top
        >>= dispatch . (Single (hash n) n)

bool :: Bool -> VM Value
bool True = here "True"
bool False = here "False"

referenceTo :: Value -> VM Value
referenceTo = fmap Reference . orefFor

doBlock :: MethodMap -> Value -> [Expr] -> VM Value
doBlock bms s es = do
    blockScope <- newObject $ \o -> o
        { oDelegates = [s]
        , oMethods = (bms, M.empty)
        }

    withTop blockScope (evalAll es)

objectFor :: Value -> VM Object
objectFor v = orefFor v >>= liftIO . readIORef

orefFor :: Value -> VM ORef
orefFor v = gets ids >>= \is -> return $ orefFrom is v

orefFrom :: IDs -> Value -> ORef
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
    lpath <- gets loadPath
    file <- findFile ("":lpath)

    alreadyLoaded <- gets ((file `elem`) . loaded)
    if alreadyLoaded
        then return ()
        else do

    case takeExtension file of
        ".atomo" -> do
            parsed <- liftIO (parseFile file)
            case parsed of
                Left e -> throwError (ParseError e)
                Right ast -> do
                    modify (\s -> s { loadPath = [path] })

                    mapM_ eval ast

                    modify $ \s -> s
                        { loadPath = lpath
                        , loaded = file : loaded s
                        }

        {-".hs" -> do-}
            {-int <- H.runInterpreter $ do-}
                {-H.loadModules [filename]-}
                {-H.setTopLevelModules ["Main"]-}
                {-H.interpret "load" (H.as :: VM ())-}

            {-load <- case int of-}
                {-Left ie -> throwError (ImportError ie)-}
                {-Right r -> return r-}

            {-load-}

        _ -> throwError . ErrorMsg $ "don't know how to load " ++ file
  where
    path = takeDirectory (normalise filename)

    findFile [] = throwError (ErrorMsg ("file not found: " ++ filename)) -- TODO: proper error
    findFile (p:ps) = do
        check <- filterM (liftIO . doesFileExist . ((p </> filename) <.>)) exts

        case check of
            [] -> findFile ps
            (ext:_) -> liftIO (canonicalizePath $ p </> filename <.> ext)

    exts = ["", "atomo", "hs"]

delegatesTo :: Value -> Value -> VM Bool
delegatesTo f t = do
    r <- orefFor f
    o <- liftIO (readIORef r)
    delegatesTo' (Reference r:oDelegates o) t
  where
    delegatesTo' [] _ = return False
    delegatesTo' (d:ds) t
        | t `elem` (d:ds) = return True
        | otherwise = do
            o <- objectFor d
            delegatesTo' (oDelegates o ++ ds) t
