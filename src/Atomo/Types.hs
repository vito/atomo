{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}
module Atomo.Types where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan
import "monads-fd" Control.Monad.Cont
import "monads-fd" Control.Monad.State
import Data.Char (isUpper)
import Data.Dynamic
import Data.Hashable (hash)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.IORef
import Text.Parsec (ParseError, SourcePos)
import Text.PrettyPrint (Doc)
import qualified Data.IntMap as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Language.Haskell.Interpreter as H

type VM = ContT Value (StateT Env IO)

data Value
    = Block !Value [Pattern] [Expr]
    | Boolean { fromBoolean :: {-# UNPACK #-} !Bool }
    | Char { fromChar :: {-# UNPACK #-} !Char }
    | Continuation { fromContinuation :: Continuation }
    | Double { fromDouble :: {-# UNPACK #-} !Double }
    | Expression { fromExpression :: Expr }
    | Haskell Dynamic
    | Integer { fromInteger :: !Integer }
    | List VVector
    | Message { fromMessage :: Message }
    | Method { fromMethod :: Method }
    | Particle { fromParticle :: Particle }
    | Process Channel ThreadId
    | Pattern { fromPattern :: Pattern }
    | Rational Rational
    | Reference
        { rORef :: {-# UNPACK #-} !ORef
        }
    | String { fromString :: !T.Text }
    deriving (Show, Typeable)

data Object =
    Object
        { oDelegates :: !Delegates
        , oMethods :: !(MethodMap, MethodMap) -- singles, keywords
        }
    deriving (Show, Typeable)

data Method
    = Responder
        { mPattern :: !Pattern
        , mContext :: !Value
        , mExpr :: !Expr
        }
    | Macro
        { mPattern :: !Pattern
        , mExpr :: !Expr
        }
    | Slot
        { mPattern :: !Pattern
        , mValue :: !Value
        }
    deriving (Eq, Show, Typeable)

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
    deriving (Eq, Show, Typeable)

data Particle
    = PMSingle String
    | PMKeyword [String] [Maybe Value]
    deriving (Eq, Show, Typeable)

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
    deriving (Show, Typeable)

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
    | PInstance Pattern
    | PStrict Pattern
    | PNamed String Pattern
    | PObject Expr
    | PPMKeyword [String] [Pattern]
    | PSingle
        { ppID :: !Int
        , ppName :: String
        , ppTarget :: Pattern
        }
    | PThis

    -- expression types, used in macros
    | PEDefine
    | PESet
    | PEDispatch
    | PEOperator
    | PEPrimitive
    | PEBlock
    | PEList
    | PEMacro
    | PEParticle
    | PETop
    | PEQuote
    | PEUnquote

    | PExpr Expr
    deriving (Show, Typeable)

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
    | EList
        { eLocation :: Maybe SourcePos
        , eContents :: [Expr]
        }
    | EMacro
        { eLocation :: Maybe SourcePos
        , ePattern :: Pattern
        , eExpr :: Expr
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
        , ePretty :: Maybe Doc
        , eAction :: VM Value
        }
    | EQuote
        { eLocation :: Maybe SourcePos
        , eExpr :: Expr
        }
    | EUnquote
        { eLocation :: Maybe SourcePos
        , eExpr :: Expr
        }
    deriving (Show, Typeable)

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
    deriving (Eq, Show, Typeable)

data EParticle
    = EPMSingle String
    | EPMKeyword [String] [Maybe Expr]
    deriving (Eq, Show, Typeable)

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
        , parserState :: ParserState
        }
    deriving Typeable

-- operator associativity
data Assoc = ALeft | ARight
    deriving (Eq, Show, Typeable)

-- a giant record of the objects for each primitive value
data IDs =
    IDs
        { idObject :: ORef -- root object
        , idBlock :: ORef
        , idBoolean :: ORef
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
        , idRational :: ORef
        , idString :: ORef
        }
    deriving (Show, Typeable)


data ParserState =
    ParserState
        { psOperators :: Operators
        , psMacros :: (MethodMap, MethodMap)
        }
    deriving (Show, Typeable)

startParserState :: ParserState
startParserState = ParserState [] (M.empty, M.empty)

-- helper synonyms
type Operators = [(String, (Assoc, Integer))] -- name -> assoc, precedence
type Delegates = [Value]
type Channel = Chan Value
type MethodMap = M.IntMap [Method]
type ORef = IORef Object
type VVector = V.Vector Value
type Continuation = IORef (Value -> VM Value)


-- a basic Eq instance
instance Eq Value where
    (==) (Block at aps aes) (Block bt bps bes) =
        at == bt && aps == bps && aes == bes
    (==) (Boolean a) (Boolean b) = a == b
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
    (==) (Rational a) (Rational b) = a == b
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
    (==) (EList _ aes) (EList _ bes) = aes == bes
    (==) (EParticle _ ap') (EParticle _ bp) = ap' == bp
    (==) (ETop _) (ETop _) = True
    (==) (EVM {}) (EVM {}) = False
    (==) _ _ = False


instance Show Channel where
    show _ = "Channel"

instance Show ORef where
    show _ = "ORef"

instance Show Continuation where
    show _ = "Continuation"

instance Show (VM a) where
    show _ = "VM"

instance Typeable (VM a) where
    typeOf _ = mkTyConApp (mkTyCon "VM") [typeOf ()]


startEnv :: Env
startEnv = Env
    { top = error "top object not set"
    , primitives =
        IDs
            { idObject = error "idObject not set"
            , idBlock = error "idBlock not set"
            , idBoolean = error "idBoolean not set"
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
            , idRational = error "idRational not set"
            , idString = error "idString not set"
            }
    , channel = error "channel not set"
    , halt = error "halt not set"
    , loadPath = []
    , loaded = []
    , stack = []
    , parserState = startParserState
    }

-- | evaluate x with e as the environment
runWith :: VM Value -> Env -> IO Value
runWith x = evalStateT (runContT x return)

-- | evaluate x with e as the environment
runVM :: VM Value -> Env -> IO (Value, Env)
runVM x = runStateT (runContT x return)


-----------------------------------------------------------------------------
-- Helpers ------------------------------------------------------------------
-----------------------------------------------------------------------------

particle :: String -> Value
{-# INLINE particle #-}
particle = Particle . PMSingle

keyParticle :: [String] -> [Maybe Value] -> Value
{-# INLINE keyParticle #-}
keyParticle ns vs = Particle $ PMKeyword ns vs

keyParticleN :: [String] -> [Value] -> Value
{-# INLINE keyParticleN #-}
keyParticleN ns vs = keyParticle ns (Nothing:map Just vs)

string :: String -> Value
{-# INLINE string #-}
string = String . T.pack

haskell :: Typeable a => a -> Value
{-# INLINE haskell #-}
haskell = Haskell . toDyn

list :: [Value] -> Value
list = List . V.fromList

fromText :: T.Text -> String
fromText = T.unpack

fromList :: Value -> [Value]
fromList (List vr) = V.toList vr
fromList v = error $ "no fromList for: " ++ show v

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
completeKP [] _ = []
completeKP (Nothing:mvs') (v:vs') = v : completeKP mvs' vs'
completeKP (Just v:mvs') vs' = v : completeKP mvs' vs'
completeKP mvs' vs' = error $ "impossible: completeKP on " ++ show (mvs', vs')

-- | Is a value a Block?
isBlock :: Value -> Bool
isBlock (Block _ _ _) = True
isBlock _ = False

-- | Is a value a Boolean?
isBoolean :: Value -> Bool
isBoolean (Boolean _) = True
isBoolean _ = False

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

-- | Is a value a Rational?
isRational :: Value -> Bool
isRational (Rational _) = True
isRational _ = False

-- | Is a value a Reference?
isReference :: Value -> Bool
isReference (Reference _) = True
isReference _ = False

-- | Is a value a String?
isString :: Value -> Bool
isString (String _) = True
isString _ = False

asValue :: AtomoError -> Value
asValue (Error v) = v
asValue (ParseError pe) =
    keyParticleN ["parse-error"] [string (show pe)]
asValue (DidNotUnderstand m) =
    keyParticleN ["did-not-understand"] [Message m]
asValue (Mismatch pat v) =
    keyParticleN
        ["pattern", "did-not-match"]
        [Pattern pat, v]
asValue (ImportError (H.UnknownError s)) =
    keyParticleN ["unknown-hint-error"] [string s]
asValue (ImportError (H.WontCompile ges)) =
    keyParticleN ["wont-compile"] [list (nub $ map (string . H.errMsg) ges)]
asValue (ImportError (H.NotAllowed s)) =
    keyParticleN ["not-allowed"] [string s]
asValue (ImportError (H.GhcException s)) =
    keyParticleN ["ghc-exception"] [string s]
asValue (FileNotFound fn) =
    keyParticleN ["file-not-found"] [string fn]
asValue (ParticleArity e' g) =
    keyParticleN
        ["particle-needed", "given"]
        [Integer (fromIntegral e'), Integer (fromIntegral g)]
asValue (BlockArity e' g) =
    keyParticleN
        ["block-expected", "given"]
        [Integer (fromIntegral e'), Integer (fromIntegral g)]
asValue NoExpressions = particle "no-expressions"
asValue (ValueNotFound d v) =
    keyParticleN ["could-not-find", "in"] [string d, v]
asValue (DynamicNeeded t) =
    keyParticleN ["dynamic-needed"] [string t]

fromHaskell' :: Typeable a => String -> Value -> a
fromHaskell' t (Haskell d) =
    fromMaybe (error ("needed Haskell value of type " ++ t))
        (fromDynamic d)
fromHaskell' t _ = error ("needed haskell value of type " ++ t)

-- convert an expression to the pattern match it represents
toPattern :: Expr -> Maybe Pattern
toPattern (Dispatch { eMessage = EKeyword { emNames = ["."], emTargets = [h, t] } }) = do
    hp <- toPattern h
    tp <- toPattern t
    return (PHeadTail hp tp)
toPattern (Dispatch { eMessage = EKeyword { emNames = ["->"], emTargets = [ETop {}, o] } }) = do
    liftM PInstance (toPattern o)
toPattern (Dispatch { eMessage = EKeyword { emNames = ["=="], emTargets = [ETop {}, o] } }) = do
    liftM PStrict (toPattern o)
toPattern (Dispatch { eMessage = EKeyword { emNames = [n], emTargets = [ETop {}, x] } }) = do
    p <- toPattern x
    return (PNamed n p)
toPattern (Dispatch { eMessage = EKeyword { emNames = ns, emTargets = ts } }) =
    return (pkeyword ns (map PObject ts))
toPattern (Dispatch { eMessage = ESingle { emName = "_" } }) =
    return PAny
toPattern (Dispatch { eMessage = ESingle { emName = n, emTarget = ETop {} } }) =
    return (PNamed n PAny)
toPattern (Dispatch { eMessage = ESingle { emTarget = d@(Dispatch {}), emName = n } }) =
    return (psingle n (PObject d))
toPattern (EList { eContents = es }) = do
    ps <- mapM toPattern es
    return (PList ps)
toPattern (EParticle { eParticle = EPMSingle n }) =
    return (PMatch (Particle (PMSingle n)))
toPattern (EParticle { eParticle = EPMKeyword ns mes }) = do
    ps <- forM mes $ \me ->
        case me of
            Nothing -> return PAny
            Just e -> toPattern e

    return (PPMKeyword ns ps)
toPattern (EQuote { eExpr = e }) = return (PExpr e)
toPattern (Primitive { eValue = v }) =
    return (PMatch v)
toPattern (ETop {}) =
    return (PObject (ETop Nothing))
toPattern b@(EBlock {}) =
    return (PObject (Dispatch Nothing (esingle "call" b)))
toPattern e = Nothing

toDefinePattern :: Expr -> Maybe Pattern
toDefinePattern (Dispatch { eMessage = ESingle { emName = n, emTarget = t } }) = do
    p <- toRolePattern t
    return (psingle n p)
toDefinePattern (Dispatch { eMessage = EKeyword { emNames = ns, emTargets = ts } }) = do
    ps <- mapM toRolePattern ts
    return (pkeyword ns ps)

toRolePattern :: Expr -> Maybe Pattern
toRolePattern (Dispatch { eMessage = EKeyword { emNames = [n], emTargets = [ETop {}, x] } }) = do
    p <- toRolePattern x
    return (PNamed n p)
toRolePattern d@(Dispatch { eMessage = ESingle { emTarget = ETop {}, emName = n } })
    | isUpper (head n) = return (PObject d)
    | otherwise = return (PNamed n PAny)
toRolePattern d@(Dispatch { eMessage = ESingle { emTarget = (Dispatch {}) } }) =
    return (PObject d)
toRolePattern p = toPattern p

toMacroPattern :: Expr -> Maybe Pattern
toMacroPattern (Dispatch { eMessage = ESingle { emName = n, emTarget = t } }) = do
    p <- toMacroRole t
    return (psingle n p)
toMacroPattern (Dispatch { eMessage = EKeyword { emNames = ns, emTargets = ts } }) = do
    ps <- mapM toMacroRole ts
    return (pkeyword ns ps)

toMacroRole :: Expr -> Maybe Pattern
toMacroRole (Dispatch _ (ESingle _ "Define" _)) = Just PEDefine
toMacroRole (Dispatch _ (ESingle _ "Set" _)) = Just PESet
toMacroRole (Dispatch _ (ESingle _ "Dispatch" _)) = Just PEDispatch
toMacroRole (Dispatch _ (ESingle _ "Operator" _)) = Just PEOperator
toMacroRole (Dispatch _ (ESingle _ "Primitive" _)) = Just PEPrimitive
toMacroRole (Dispatch _ (ESingle _ "Block" _)) = Just PEBlock
toMacroRole (Dispatch _ (ESingle _ "List" _)) = Just PEList
toMacroRole (Dispatch _ (ESingle _ "Macro" _)) = Just PEMacro
toMacroRole (Dispatch _ (ESingle _ "Particle" _)) = Just PEParticle
toMacroRole (Dispatch _ (ESingle _ "Top" _)) = Just PETop
toMacroRole (Dispatch _ (ESingle _ "Quote" _)) = Just PEQuote
toMacroRole (Dispatch _ (ESingle _ "Unquote" _)) = Just PEUnquote
toMacroRole (Dispatch { eMessage = EKeyword { emNames = [n], emTargets = [ETop {}, x] } }) = do
    p <- toMacroRole x
    return (PNamed n p)
toMacroRole (ETop {}) = Just PAny
toMacroRole p = toPattern p
