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
    = SingleParticle String
    | KeywordParticle [String] [Maybe Value]
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
    | PKeyword
        { ppID :: !Int
        , ppNames :: [String]
        , ppTargets :: [Pattern]
        }
    | PMatch Value
    | PNamed String Pattern
    | PObject Expr
    | PPSingle String
    | PPKeyword [String] [Pattern]
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
    {-| EDispatchObject-}
        {-{ eLocation :: Maybe SourcePos-}
        {-}-}
    | EVM
        { eLocation :: Maybe SourcePos
        , eAction :: VM Value
        }
    | EList
        { eLocation :: Maybe SourcePos
        , eContents :: [Expr]
        }
    | ETop
        { eLocation :: Maybe SourcePos
        }
    | EParticle
        { eLocation :: Maybe SourcePos
        , eParticle :: EParticle
        }
    | EMatch
        { eLocation :: Maybe SourcePos
        , eTarget :: Expr
        , eBranches :: [(Pattern, Expr)]
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
    = EPSingle String
    | EPKeyword [String] [Maybe Expr]
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
particle = Particle . SingleParticle

insertMethod :: Method -> MethodMap -> MethodMap
insertMethod m mm =
    M.insertWith (flip (++)) key [m] mm -- TODO: insert by precision
    {-case M.lookup key mm of-}
        {-Nothing -> M.insert key [m] mm-}
        {-Just ms -> M.insert key (ms ++ [m]) mm -- TODO: precision comparison-}
  where
    key = ppID (mPattern m)

toMethods :: [(Pattern, Value)] -> MethodMap
toMethods bs = foldl (\ss (p, v) -> insertMethod (Slot p v) ss) M.empty bs
