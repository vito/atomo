{-# LANGUAGE BangPatterns #-}
module Atomo.Environment where

import Control.Monad.Error
import Control.Monad.State
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Data.IORef
import Data.Hashable
import Data.List (nub)
import System.IO.Unsafe
import qualified Data.IntMap as M

import Atomo.Types


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

run :: VM () -> IO (Either AtomoError ())
run x = runWith (initEnv >> x) startEnv

runWith :: VM a -> Env -> IO (Either AtomoError a)
runWith x s = evalStateT (runErrorT x) s

-- | set up the primitive objects, etc.
initEnv :: VM ()
{-# INLINE initEnv #-}
initEnv = do
    object <- newObject id

    topObj <- newObject $ \o -> o { oDelegates = oDelegates o ++ [object] }
    modify $ \e -> e { top = topObj }

    define (PSingle (hash "Object") "Object" (PMatch topObj)) (Primitive Nothing object)

    integer <- newObject $ \o -> o { oDelegates = oDelegates o ++ [object] }
    define (PSingle (hash "Integer") "Integer" (PMatch topObj)) (Primitive Nothing integer)

    modify $ \e -> e
        { ids = IDs
            { idObject = rORef object
            , idInteger = rORef integer
            }
        }



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
    eval' (Set { ePattern = p, eExpr = ev }) = do
        v <- eval ev

        forM_ (bindings' p v) $ \(p', v') -> do
            define p' (Primitive (eLocation ev) v')

        return v
    eval' (Dispatch { eMessage = ESingle { emID = i, emName = n, emTarget = t } }) = do
        v <- eval t
        dispatch (Single i n v)
    eval' (Dispatch { eMessage = EKeyword { emID = i, emNames = ns, emTargets = ts } }) = do
        vs <- mapM eval ts
        dispatch (Keyword i ns vs)
    eval' (Primitive { eValue = v }) = return v
    eval' (ETop {}) = gets top
    eval' (EVM { eAction = x }) = x
    eval' _ = error $ "no eval for " ++ show e

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
                    PSingle {} -> (insertMethod m oss, oks)
                    PKeyword {} -> (oss, insertMethod m oks)

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
targets is (PMatch (Reference o)) = return [o]
targets is (PMatch (Integer _)) = return [idInteger is]
targets is (PSingle _ _ p) = targets is p
targets is (PKeyword _ _ ps) = do
    ts <- mapM (targets is) ps
    return (nub (concat ts))
targets is (PNamed _ p) = targets is p
targets is PSelf = gets top >>= orefFor >>= return . (: [])
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
match ids (PMatch (Reference x)) (Integer _) =
    match ids (PMatch (Reference x)) (Reference (idInteger ids))
match ids (PMatch (Integer x)) (Integer y) =
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
bindings' (PPKeyword _ ps) (Particle (KeywordParticle _ mvs)) = concat
    $ map (\(p, Just v) -> bindings' p v)
    $ filter (\(_, mv) -> case mv of { Nothing -> False; _ -> True })
    $ zip ps mvs
bindings' _ _ = []

objectFor :: Value -> VM Object
objectFor v = orefFor v >>= liftIO . readIORef

orefFor :: Value -> VM ORef
orefFor (Reference r) = return r
orefFor (Integer _) = gets (idInteger . ids)
