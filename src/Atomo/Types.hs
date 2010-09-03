{-# LANGUAGE BangPatterns, TypeSynonymInstances #-}
module Atomo.Types where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan
import Control.Monad.Error
import Control.Monad.State
import Data.Dynamic
import Data.IORef
import Text.Parsec (ParseError, SourcePos)
import qualified Data.Vector as V
import qualified Data.IntMap as M
{-import qualified Language.Haskell.Interpreter as H-}

type VM = ErrorT AtomoError (StateT Env IO)

data Value
    = Block !Value [Pattern] [Expr]
    | Char {-# UNPACK #-} !Char
    | Double {-# UNPACK #-} !Double
    | Expression Expr
    | Haskell Dynamic
    | Integer !Integer
    | List VVector
    | Message Message
    | Particle Particle
    | Process Channel ThreadId
    | Pattern Pattern
    | Reference
        { rORef :: {-# UNPACK #-} !ORef
        }
    deriving Show

data Object =
    Object
        { oDelegates :: ![Value]
        , oMethods :: !(MethodMap, MethodMap) -- singles, keywords
        }
    deriving Show

data Method
    = Method
        { mPattern :: !Pattern
        , mTop :: !Value
        , mExpr :: !Expr
        }
    | Slot
        { mPattern :: !Pattern
        , mValue :: !Value
        }
    deriving Show

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
    deriving Show

data Particle
    = PMSingle String
    | PMKeyword [String] [Maybe Value]
    deriving Show

data AtomoError
    = ErrorMsg String
    | ParseError ParseError
    | DidNotUnderstand Message
    {-| ImportError H.InterpreterError-}
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
    | PPMSingle String
    | PPMKeyword [String] [Pattern]
    | PSelf
    | PSingle
        { ppID :: !Int
        , ppName :: String
        , ppTarget :: Pattern
        }
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
    | EVM
        { eLocation :: Maybe SourcePos
        , eAction :: VM Value
        }
    deriving Show

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
    deriving Show

data EParticle
    = EPMSingle String
    | EPMKeyword [String] [Maybe Expr]
    deriving Show

-- the evaluation environment
data Env =
    Env
        { top :: Value
        , ids :: IDs
        , channel :: Channel
        , halt :: IO ()
        , loadPath :: [FilePath]
        , loaded :: [FilePath]
        , stack :: [Expr]
        , call :: Call
        }

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
        , idDouble :: ORef
        , idExpression :: ORef
        , idInteger :: ORef
        , idList :: ORef
        , idMessage :: ORef
        , idParticle :: ORef
        , idProcess :: ORef
        , idPattern :: ORef
        }


instance Error AtomoError where
    noMsg = ErrorMsg ""
    strMsg = ErrorMsg


-- a basic Eq instance
instance Eq Value where
    Char a == Char b = a == b
    Double a == Double b = a == b
    Integer a == Integer b = a == b
    List a == List b = a == b
    Process _ a == Process _ b = a == b
    Reference a == Reference b = a == b
    _ == _ = False

-- helper synonyms
type Channel = Chan Value
type MethodMap = M.IntMap [Method]
type ORef = IORef Object
type VVector = IORef (V.Vector Value)

instance Show Channel where
    show _ = "Channel"

instance Show ORef where
    show _ = "ORef"

instance Show VVector where
    show _ = "VVector"

instance Show (VM a) where
    show _ = "VM"


startEnv :: Env
startEnv = Env
    { top = error "top object not set"
    , ids =
        IDs
            { idMatch = error "idMatch not set"
            , idObject = error "idObject not set"
            , idBlock = error "idBlock not set"
            , idChar = error "idChar not set"
            , idDouble = error "idDouble not set"
            , idExpression = error "idExpression not set"
            , idInteger = error "idInteger not set"
            , idList = error "idList not set"
            , idMessage = error "idMessage not set"
            , idParticle = error "idParticle not set"
            , idProcess = error "idProcess not set"
            , idPattern = error "idPattern not set"
            }
    , channel = error "channel not set"
    , halt = error "halt not set"
    , loadPath = []
    , loaded = []
    , stack = []
    , call = error "call not set"
    }

particle :: String -> Value
{-# INLINE particle #-}
particle = Particle . PMSingle

keyParticle :: [String] -> [Maybe Value] -> Value
{-# INLINE keyParticle #-}
keyParticle ns vs = Particle $ PMKeyword ns vs


-----------------------------------------------------------------------------
-- Helpers ------------------------------------------------------------------
-----------------------------------------------------------------------------

string :: MonadIO m => String -> m Value
string = list . map Char

list :: MonadIO m => [Value] -> m Value
list = list' . V.fromList

list' :: MonadIO m => V.Vector Value -> m Value
list' = liftM List . liftIO . newIORef

-- | Is a value a Block?
isBlock :: Value -> Bool
isBlock (Block _ _ _) = True
isBlock _ = False

-- | Is a value a Char?
isChar :: Value -> Bool
isChar (Char _) = True
isChar _ = False

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

