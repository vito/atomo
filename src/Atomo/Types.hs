{-# LANGUAGE BangPatterns, ExistentialQuantification, TypeSynonymInstances #-}
module Atomo.Types where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Monad.Identity
import Data.Dynamic
import Data.Hashable (hash)
import Data.IORef
import Data.Typeable
import Text.Parsec (ParseError, SourcePos)
import Unsafe.Coerce
import qualified Data.IntMap as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Language.Haskell.Interpreter as H

newtype VMT r m a =
    VM
        { runVM :: Env -> ContT (Either AtomoError r, Env) m (Either AtomoError a, Env)
        }

type VM = VMT Value IO

instance Monad m => Monad (VMT r m) where
    x >>= f = VM $ \e -> do
        (ev, e') <- runVM x e
        case ev of
            Right v -> runVM (f v) e'
            Left err -> return (Left err, e')

    return x = VM $ \e -> return (Right x, e)

instance MonadTrans (VMT r) where
    lift m = VM $ \e -> do
        a <- lift m
        return (Right a, e)

instance MonadIO m => MonadIO (VMT r m) where
    liftIO f = VM $ \e -> do
        x <- liftIO f
        return (Right x, e)

instance Monad m => MonadCont (VMT r m) where
    callCC f = VM $ \e ->
        callCC $ \c -> runVM (f (\a -> VM (\e' -> c (Right a, e')))) e


data Value
    = Block !Value [Pattern] [Expr]
    | Char {-# UNPACK #-} !Char
    | Continuation Continuation
    | Double {-# UNPACK #-} !Double
    | Expression Expr
    | Haskell Dynamic
    | Integer !Integer
    | List VVector
    | Message Message
    | Method Method
    | Particle Particle
    | Process Channel ThreadId
    | Pattern Pattern
    | Reference
        { rORef :: {-# UNPACK #-} !ORef
        }
    | String !T.Text
    deriving Show

data Object =
    Object
        { oDelegates :: !Delegates
        , oMethods :: !(MethodMap, MethodMap) -- singles, keywords
        }
    deriving Show

data Method
    = Responder
        { mPattern :: !Pattern
        , mContext :: !Value
        , mExpr :: !Expr
        }
    | Slot
        { mPattern :: !Pattern
        , mValue :: !Value
        }
    deriving (Eq, Show)

data Message
    = Keyword
        { mID :: !Int
        , mNames :: [String]
        , mTargets :: [Value]
        }
    | Single
        { mID :: !Int
        , mName :: String
        , mTarget :: Value
        }
    deriving (Eq, Show)

data Particle
    = PMSingle String
    | PMKeyword [String] [Maybe Value]
    deriving (Eq, Show)

data AtomoError
    = Error Value
    | ParseError ParseError
    | DidNotUnderstand Message
    | Mismatch Pattern Value
    | ImportError H.InterpreterError
    | FileNotFound String
    | ParticleArity Int Int
    | BlockArity Int Int
    | NoExpressions
    | ValueNotFound String Value
    | DynamicNeeded String
    deriving Show

-- pattern-matches
data Pattern
    = PAny
    | PHeadTail Pattern Pattern
    | PKeyword
        { ppID :: !Int
        , ppNames :: [String]
        , ppTargets :: [Pattern]
        }
    | PList [Pattern]
    | PMatch Value
    | PNamed String Pattern
    | PObject Expr
    | PPMKeyword [String] [Pattern]
    | PSingle
        { ppID :: !Int
        , ppName :: String
        , ppTarget :: Pattern
        }
    | PThis
    deriving Show

-- expressions
data Expr
    = Define
        { eLocation :: Maybe SourcePos
        , ePattern :: Pattern
        , eExpr :: Expr
        }
    | Set
        { eLocation :: Maybe SourcePos
        , ePattern :: Pattern
        , eExpr :: Expr
        }
    | Dispatch
        { eLocation :: Maybe SourcePos
        , eMessage :: EMessage
        }
    | Operator
        { eLocation :: Maybe SourcePos
        , eNames :: [String]
        , eAssoc :: Assoc
        , ePrec :: Integer
        }
    | Primitive
        { eLocation :: Maybe SourcePos
        , eValue :: !Value
        }
    | EBlock
        { eLocation :: Maybe SourcePos
        , eArguments :: [Pattern]
        , eContents :: [Expr]
        }
    | EDispatchObject
        { eLocation :: Maybe SourcePos
        }
    | EList
        { eLocation :: Maybe SourcePos
        , eContents :: [Expr]
        }
    | EParticle
        { eLocation :: Maybe SourcePos
        , eParticle :: EParticle
        }
    | ETop
        { eLocation :: Maybe SourcePos
        }
    | forall r. EVM
        { eLocation :: Maybe SourcePos
        , eAction :: VMT r IO Value
        }

data EMessage
    = EKeyword
        { emID :: !Int
        , emNames :: [String]
        , emTargets :: [Expr]
        }
    | ESingle
        { emID :: !Int
        , emName :: String
        , emTarget :: Expr
        }
    deriving (Eq, Show)

data EParticle
    = EPMSingle String
    | EPMKeyword [String] [Maybe Expr]
    deriving (Eq, Show)

-- the evaluation environment
data Env =
    Env
        { top :: Value
        , primitives :: IDs
        , channel :: Channel
        , halt :: IO ()
        , loadPath :: [FilePath]
        , loaded :: [FilePath]
        , stack :: [Expr]
        , call :: Call
        , parserState :: Operators
        }

-- operator associativity
data Assoc = ALeft | ARight
    deriving (Eq, Show)

-- meta information for the dispatch
data Call =
    Call
        { callSender :: Value
        , callMessage :: Message
        , callContext :: Value
        }

-- a giant record of the objects for each primitive value
data IDs =
    IDs
        { idMatch :: ORef -- used in dispatch to refer to the object currently being searched
        , idObject :: ORef -- root object
        , idBlock :: ORef
        , idChar :: ORef
        , idContinuation :: ORef
        , idDouble :: ORef
        , idExpression :: ORef
        , idHaskell :: ORef
        , idInteger :: ORef
        , idList :: ORef
        , idMessage :: ORef
        , idMethod :: ORef
        , idParticle :: ORef
        , idProcess :: ORef
        , idPattern :: ORef
        , idString :: ORef
        }


-- helper synonyms
type Operators = [(String, (Assoc, Integer))] -- name -> assoc, precedence
type Delegates = [Value]
type Channel = Chan Value
type MethodMap = M.IntMap [Method]
type ORef = IORef Object
type VVector = IORef (V.Vector Value)

data Continuation = forall m r. ContinuationValue (IORef (Value -> VMT r m Value))

instance Eq Continuation where
    ContinuationValue a == ContinuationValue b = a == unsafeCoerce b


instance Show Expr where
    show = const "TODO"


-- a basic Eq instance
instance Eq Value where
    (==) (Block at aps aes) (Block bt bps bes) =
        at == bt && aps == bps && aes == bes
    (==) (Char a) (Char b) = a == b
    (==) (Continuation a) (Continuation b) = a == b
    (==) (Double a) (Double b) = a == b
    (==) (Expression a) (Expression b) = a == b
    (==) (Haskell _) (Haskell _) = False
    (==) (Integer a) (Integer b) = a == b
    (==) (List a) (List b) = a == b
    (==) (Message a) (Message b) = a == b
    (==) (Method a) (Method b) = a == b
    (==) (Particle a) (Particle b) = a == b
    (==) (Process _ a) (Process _ b) = a == b
    (==) (Reference a) (Reference b) = a == b
    (==) (String a) (String b) = a == b
    (==) _ _ = False


instance Eq Pattern where
    -- check if two patterns are "equivalent", ignoring names for patterns
    -- and other things that mean the same thing
    (==) PAny PAny = True
    (==) (PHeadTail ah at) (PHeadTail bh bt) =
        (==) ah bh && (==) at bt
    (==) (PKeyword _ ans aps) (PKeyword _ bns bps) =
        ans == bns && and (zipWith (==) aps bps)
    (==) (PList aps) (PList bps) =
        length aps == length bps && and (zipWith (==) aps bps)
    (==) (PMatch a) (PMatch b) = a == b
    (==) (PNamed _ a) (PNamed _ b) = (==) a b
    (==) (PNamed _ a) b = (==) a b
    (==) a (PNamed _ b) = (==) a b
    (==) (PPMKeyword ans aps) (PPMKeyword bns bps) =
        ans == bns && and (zipWith (==) aps bps)
    (==) (PSingle ai _ at) (PSingle bi _ bt) =
        ai == bi && (==) at bt
    (==) PThis PThis = True
    (==) _ _ = False


instance Eq Expr where
    (==) (Define _ ap' ae) (Define _ bp be) = ap' == bp && ae == be
    (==) (Set _ ap' ae) (Set _ bp be) = ap' == bp && ae == be
    (==) (Dispatch _ am) (Dispatch _ bm) = am == bm
    (==) (Operator _ ans aa ap') (Operator _ bns ba bp) =
        ans == bns && aa == ba && ap' == bp
    (==) (Primitive _ a) (Primitive _ b) = a == b
    (==) (EBlock _ aas aes) (EBlock _ bas bes) =
        aas == bas && aes == bes
    (==) (EDispatchObject _) (EDispatchObject _) = True
    (==) (EList _ aes) (EList _ bes) = aes == bes
    (==) (EParticle _ ap') (EParticle _ bp) = ap' == bp
    (==) (ETop _) (ETop _) = True
    (==) (EVM _ _) (EVM _ _) = False
    (==) _ _ = False



instance Show Channel where
    show _ = "Channel"

instance Show ORef where
    show _ = "ORef"

instance Show VVector where
    show _ = "VVector"

instance Show Continuation where
    show _ = "Continuation"

instance Show (VMT r m a) where
    show _ = "VM"

instance Typeable (VMT r m a) where
    typeOf _ = mkTyConApp (mkTyCon "VM") [typeOf ()]


startEnv :: Env
startEnv = Env
    { top = error "top object not set"
    , primitives =
        IDs
            { idMatch = error "idMatch not set"
            , idObject = error "idObject not set"
            , idBlock = error "idBlock not set"
            , idChar = error "idChar not set"
            , idContinuation = error "idContinuation not set"
            , idDouble = error "idDouble not set"
            , idExpression = error "idExpression not set"
            , idHaskell = error "idHaskell not set"
            , idInteger = error "idInteger not set"
            , idList = error "idList not set"
            , idMessage = error "idMessage not set"
            , idMethod = error "idMethod not set"
            , idParticle = error "idParticle not set"
            , idProcess = error "idProcess not set"
            , idPattern = error "idPattern not set"
            , idString = error "idString not set"
            }
    , channel = error "channel not set"
    , halt = error "halt not set"
    , loadPath = []
    , loaded = []
    , stack = []
    , call = error "call not set"
    , parserState = []
    }



throwError :: Monad m => AtomoError -> VMT r m b
throwError err = VM $ \e -> return (Left err, e)

catchError :: Monad m => VMT r m a -> (AtomoError -> VMT r m a) -> VMT r m a
catchError x h = VM $ \e -> do
    (r, e') <- runVM x e
    case r of
        Left err -> runVM (h err) e
        Right ok -> return (Right ok, e')

get :: Monad m => VMT r m Env
get = VM $ \e -> return (Right e, e)

gets :: Monad m => (Env -> a) -> VMT r m a
gets f = VM $ \e -> return (Right (f e), e)

put :: Monad m => Env -> VMT r m ()
put e = VM $ \_ -> return (Right (), e)

modify :: Monad m => (Env -> Env) -> VMT r m ()
modify f = get >>= put . f

-----------------------------------------------------------------------------
-- Helpers ------------------------------------------------------------------
-----------------------------------------------------------------------------

vmIO :: MonadIO m => VMT a IO a -> VMT r m a
vmIO x = VM $ \e -> liftIO (runContT (runVM x e) return)

particle :: String -> Value
{-# INLINE particle #-}
particle = Particle . PMSingle

keyParticle :: [String] -> [Maybe Value] -> Value
{-# INLINE keyParticle #-}
keyParticle ns vs = Particle $ PMKeyword ns vs

keyParticleN :: [String] -> [Value] -> Value
{-# INLINE keyParticleN #-}
keyParticleN ns vs = keyParticle ns (Nothing:map Just vs)

raise :: Monad m => [String] -> [Value] -> VMT r m a
{-# INLINE raise #-}
raise ns vs = throwError . Error $ keyParticleN ns vs

raise' :: Monad m => String -> VMT r m a
{-# INLINE raise' #-}
raise' n = throwError . Error $ particle n

string :: String -> Value
{-# INLINE string #-}
string = String . T.pack

haskell :: Typeable a => a -> Value
{-# INLINE haskell #-}
haskell = Haskell . toDyn

fromHaskell :: (Monad m, Typeable a) => String -> Value -> VMT r m a
fromHaskell t (Haskell d) =
    case fromDynamic d of
        Just a -> return a
        Nothing -> throwError (DynamicNeeded t)
fromHaskell t _ = throwError (DynamicNeeded t)

fromHaskell' :: Typeable a => String -> Value -> a
fromHaskell' t (Haskell d) =
    case fromDynamic d of
        Just a -> a
        Nothing -> error ("needed Haskell value of type " ++ t)
fromHaskell' t _ = error ("needed haskell value of type " ++ t)

list :: MonadIO m => [Value] -> m Value
list = list' . V.fromList

list' :: MonadIO m => V.Vector Value -> m Value
list' = liftM List . liftIO . newIORef

fromString :: Value -> String
fromString (String s) = T.unpack s
fromString v = error $ "no fromString for: " ++ show v

toList :: MonadIO m => Value -> m [Value]
toList (List vr) = liftM V.toList (liftIO (readIORef vr))
toList v = error $ "no toList for: " ++ show v

single :: String -> Value -> Message
{-# INLINE single #-}
single n = Single (hash n) n

keyword :: [String] -> [Value] -> Message
{-# INLINE keyword #-}
keyword ns = Keyword (hash ns) ns

psingle :: String -> Pattern -> Pattern
{-# INLINE psingle #-}
psingle n = PSingle (hash n) n

pkeyword :: [String] -> [Pattern] -> Pattern
{-# INLINE pkeyword #-}
pkeyword ns = PKeyword (hash ns) ns

esingle :: String -> Expr -> EMessage
{-# INLINE esingle #-}
esingle n = ESingle (hash n) n

ekeyword :: [String] -> [Expr] -> EMessage
{-# INLINE ekeyword #-}
ekeyword ns = EKeyword (hash ns) ns

-- | Fill in the empty values of a particle. The number of values missing
-- is expected to be equal to the number of values provided.
completeKP :: [Maybe Value] -> [Value] -> [Value]
completeKP [] [] = []
completeKP (Nothing:mvs') (v:vs') = v : completeKP mvs' vs'
completeKP (Just v:mvs') vs' = v : completeKP mvs' vs'
completeKP mvs' vs' = error $ "impossible: completeKP on " ++ show (mvs', vs')

-- | Is a value a Block?
isBlock :: Value -> Bool
isBlock (Block _ _ _) = True
isBlock _ = False

-- | Is a value a Char?
isChar :: Value -> Bool
isChar (Char _) = True
isChar _ = False

-- | Is a value a Continuation?
isContinuation :: Value -> Bool
isContinuation (Continuation _) = True
isContinuation _ = False

-- | Is a value a Double?
isDouble :: Value -> Bool
isDouble (Double _) = True
isDouble _ = False

-- | Is a value an Expression?
isExpression :: Value -> Bool
isExpression (Expression _) = True
isExpression _ = False

-- | Is a value a Haskell value?
isHaskell :: Value -> Bool
isHaskell (Haskell _) = True
isHaskell _ = False

-- | Is a value an Integer?
isInteger :: Value -> Bool
isInteger (Integer _) = True
isInteger _ = False

-- | Is a value a List?
isList :: Value -> Bool
isList (List _) = True
isList _ = False

-- | Is a value a Message?
isMessage :: Value -> Bool
isMessage (Message _) = True
isMessage _ = False

-- | Is a value a Method?
isMethod :: Value -> Bool
isMethod (Method _) = True
isMethod _ = False

-- | Is a value a Particle?
isParticle :: Value -> Bool
isParticle (Particle _) = True
isParticle _ = False

-- | Is a value a Pattern?
isPattern :: Value -> Bool
isPattern (Pattern _) = True
isPattern _ = False

-- | Is a value a Process?
isProcess :: Value -> Bool
isProcess (Process _ _) = True
isProcess _ = False

-- | Is a value a Reference?
isReference :: Value -> Bool
isReference (Reference _) = True
isReference _ = False

-- | Is a value a String?
isString :: Value -> Bool
isString (String _) = True
isString _ = False
