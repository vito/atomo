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


-- | The Atomo VM. A Continuation monad wrapping a State monad.
type VM = ContT Value (StateT Env IO)

-- | All values usable in Atomo.
data Value
    -- | A block of expressions, bound to a context and with optional arguments.
    = Block !Value [Pattern] [Expr]

    -- | A boolean value.
    | Boolean { fromBoolean :: {-# UNPACK #-} !Bool }

    -- | A character value.
    | Char { fromChar :: {-# UNPACK #-} !Char }

    -- | A continuation reference.
    | Continuation { fromContinuation :: Continuation }

    -- | A double value.
    | Double { fromDouble :: {-# UNPACK #-} !Double }

    -- | An expression value.
    | Expression { fromExpression :: Expr }

    -- | A dynamically-typed Haskell value.
    | Haskell Dynamic

    -- | An Integer value.
    | Integer { fromInteger :: !Integer }

    -- | A vector of Values.
    | List VVector

    -- | A message value.
    | Message { fromMessage :: Message }

    -- | A method value.
    | Method { fromMethod :: Method }

    -- | A particle value.
    | Particle { fromParticle :: Particle }

    -- | A process; a communications channel and the thread's ID.
    | Process Channel ThreadId

    -- | A pattern value.
    | Pattern { fromPattern :: Pattern }

    -- | A rational value.
    | Rational Rational

    -- | An object reference.
    | Reference
        { rORef :: {-# UNPACK #-} !ORef
        }

    -- | A string value; Data.Text.Text.
    | String { fromString :: !T.Text }
    deriving (Show, Typeable)

-- | A pure object.
data Object =
    Object
        { -- | The object's delegates list.
          oDelegates :: !Delegates

          -- | A pair of (single, keyword) methods.
        , oMethods :: !(MethodMap, MethodMap)
        }
    deriving (Show, Typeable)

-- | Methods, slot, and macro methods.
data Method
    -- | Responds to a message by evaluating an expression in the given context.
    = Responder
        { mPattern :: !Pattern
        , mContext :: !Value
        , mExpr :: !Expr
        }

    -- | Responds to a macro message by evaluating an expression.
    | Macro
        { mPattern :: !Pattern
        , mExpr :: !Expr
        }

    -- | Responds to a message by returning a value.
    | Slot
        { mPattern :: !Pattern
        , mValue :: !Value
        }
    deriving (Eq, Show, Typeable)

-- | Messages sent to objects.
data Message
    -- | A keyword-delimited message with multiple targets.
    = Keyword
        { mID :: !Int
        , mNames :: [String]
        , mTargets :: [Value]
        }

    -- | A single message sent to one target.
    | Single
        { mID :: !Int
        , mName :: String
        , mTarget :: Value
        }
    deriving (Eq, Show, Typeable)

-- | Partial messages.
data Particle
    -- | A single message with no target.
    = PMSingle String

    -- | A keyword message with many optional targets.
    | PMKeyword [String] [Maybe Value]
    deriving (Eq, Show, Typeable)

-- | Shortcut error values.
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

-- | Pattern-matching.
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

-- | Expressions; the nodes in a syntax tree.
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
    | EForMacro
        { eLocation :: Maybe SourcePos
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

-- | An unevaluated message dispatch.
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

-- | An unevaluated particle.
data EParticle
    = EPMSingle String
    | EPMKeyword [String] [Maybe Expr]
    deriving (Eq, Show, Typeable)

-- | Atomo's VM state.
data Env =
    Env
        { -- | The current toplevel object.
          top :: Value

          -- | A record of objects representing the primitive values.
        , primitives :: IDs

          -- | This process's communications channel.
        , channel :: Channel

          -- | Function to call which will shut down the entire VM when called
          -- from any thread.
        , halt :: IO ()

          -- | Paths to search for files.
        , loadPath :: [FilePath]

          -- | Files aready loaded.
        , loaded :: [FilePath]

          -- | The last N expressions evaluated up to the current error.
        , stack :: [Expr]

          -- | The parser's state, passed around when a parser action needs to
          -- be run.
        , parserState :: ParserState
        }
    deriving Typeable

-- | Operator associativity.
data Assoc = ALeft | ARight
    deriving (Eq, Show, Typeable)

-- | A giant record of the objects for each primitive value.
data IDs =
    IDs
        { idObject :: ORef
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

-- | State underlying the parser, and saved in the VM.
data ParserState =
    ParserState
        { -- | Operator precedence and associativity.
          psOperators :: Operators

          -- | All defined macros; (single macros, keyword macros).
        , psMacros :: (MethodMap, MethodMap)

          -- | Are we parsing in a quasiquote?
        , psInQuote :: Bool

          -- | The number of macros we've expanded.
        , psClock :: Integer
        }
    deriving (Show, Typeable)

-- | A map of operators to their associativity and precedence.
type Operators = [(String, (Assoc, Integer))]

-- | The list of values an object delegates to.
type Delegates = [Value]

-- | A channel for sending values through to processes.
type Channel = Chan Value

-- | A selector-indexed and precision-sorted map of methods.
type MethodMap = M.IntMap [Method]

-- | A reference to an object.
type ORef = IORef Object

-- | A vector containing values.
type VVector = V.Vector Value

-- | A reference to a continuation function.
type Continuation = IORef (Value -> VM Value)


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


-- | Initial "empty" parser state.
startParserState :: ParserState
startParserState = ParserState [] (M.empty, M.empty) False 0

-- | Initial "empty" environment state.
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

-- | Evaluate x with e as the environment.
runWith :: VM Value -> Env -> IO Value
runWith x = evalStateT (runContT x return)

-- | Evaluate x with e as the environment.
runVM :: VM Value -> Env -> IO (Value, Env)
runVM x = runStateT (runContT x return)


-----------------------------------------------------------------------------
-- Helpers ------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Create a single particle with a given name.
particle :: String -> Value
{-# INLINE particle #-}
particle = Particle . PMSingle

-- | Create a keyword particle with a given name and optional values.
keyParticle :: [String] -> [Maybe Value] -> Value
{-# INLINE keyParticle #-}
keyParticle ns vs = Particle $ PMKeyword ns vs

-- | Create a keyword particle with no first role and the given values.
keyParticleN :: [String] -> [Value] -> Value
{-# INLINE keyParticleN #-}
keyParticleN ns vs = keyParticle ns (Nothing:map Just vs)

-- | Convert a String into a Value.
string :: String -> Value
{-# INLINE string #-}
string = String . T.pack

-- | Convert a Typeable value into a Haskell Value.
haskell :: Typeable a => a -> Value
{-# INLINE haskell #-}
haskell = Haskell . toDyn

-- | Convert a list of values into a List Value.
list :: [Value] -> Value
{-# INLINE list #-}
list = List . V.fromList

-- | Convert a Text string into a String.
fromText :: T.Text -> String
{-# INLINE fromText #-}
fromText = T.unpack

-- | Convert a List Value into a list of Values.
fromList :: Value -> [Value]
fromList (List vr) = V.toList vr
fromList v = error $ "no fromList for: " ++ show v

-- | Create a single message with a given name and target.
single :: String -> Value -> Message
{-# INLINE single #-}
single n = Single (hash n) n

-- | Create a keyword message with a given name and targets.
keyword :: [String] -> [Value] -> Message
{-# INLINE keyword #-}
keyword ns = Keyword (hash ns) ns

-- | Create a single message pattern with a given name and target pattern.
psingle :: String -> Pattern -> Pattern
{-# INLINE psingle #-}
psingle n = PSingle (hash n) n

-- | Create a keyword message pattern with a given name and target patterns.
pkeyword :: [String] -> [Pattern] -> Pattern
{-# INLINE pkeyword #-}
pkeyword ns = PKeyword (hash ns) ns

-- | Create a single message expression with a given name and target expression.
esingle :: String -> Expr -> EMessage
{-# INLINE esingle #-}
esingle n = ESingle (hash n) n

-- | Create a keyword message expression with a given name and target expressions.
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

-- | Convert an AtomoError into the Value we want to error with.
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

-- | Convert an Atomo Haskell dynamic value into its value, erroring on
-- failure.
fromHaskell' :: Typeable a => String -> Value -> a
fromHaskell' t (Haskell d) =
    fromMaybe (error ("needed Haskell value of type " ++ t))
        (fromDynamic d)
fromHaskell' t _ = error ("needed haskell value of type " ++ t)

-- | Convert an expression to the pattern match it represents.
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
toPattern _ = Nothing

-- | Convert an expression into a definition's message pattern.
toDefinePattern :: Expr -> Maybe Pattern
toDefinePattern (Dispatch { eMessage = ESingle { emName = n, emTarget = t } }) = do
    p <- toRolePattern t
    return (psingle n p)
toDefinePattern (Dispatch { eMessage = EKeyword { emNames = ns, emTargets = ts } }) = do
    ps <- mapM toRolePattern ts
    return (pkeyword ns ps)

-- | Convert an expression into a pattern-match for use as a message's role.
toRolePattern :: Expr -> Maybe Pattern
toRolePattern (Dispatch { eMessage = EKeyword { emNames = ["->"], emTargets = [ETop {}, o] } }) = do
    liftM PInstance (toRolePattern o)
toRolePattern (Dispatch { eMessage = EKeyword { emNames = ["=="], emTargets = [ETop {}, o] } }) = do
    liftM PStrict (toRolePattern o)
toRolePattern (Dispatch { eMessage = EKeyword { emNames = [n], emTargets = [ETop {}, x] } }) = do
    p <- toRolePattern x
    return (PNamed n p)
toRolePattern d@(Dispatch { eMessage = ESingle { emTarget = ETop {}, emName = n } })
    | isUpper (head n) = return (PObject d)
    | otherwise = return (PNamed n PAny)
toRolePattern d@(Dispatch { eMessage = ESingle { emTarget = (Dispatch {}) } }) =
    return (PObject d)
toRolePattern p = toPattern p

-- | Convert an expression into a macro's message pattern.
toMacroPattern :: Expr -> Maybe Pattern
toMacroPattern (Dispatch { eMessage = ESingle { emName = n, emTarget = t } }) = do
    p <- toMacroRole t
    return (psingle n p)
toMacroPattern (Dispatch { eMessage = EKeyword { emNames = ns, emTargets = ts } }) = do
    ps <- mapM toMacroRole ts
    return (pkeyword ns ps)

-- | Convert an expression into a pattern-match for use as a macro's role.
toMacroRole :: Expr -> Maybe Pattern
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
