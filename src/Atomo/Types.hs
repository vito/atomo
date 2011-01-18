{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances #-}
module Atomo.Types where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan
import Control.Monad.Cont
import Control.Monad.State
import Data.Dynamic
import Data.Hashable (hash)
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Data.IORef
import Text.Parsec (ParseError, SourcePos, sourceName, sourceLine, sourceColumn)
import Text.PrettyPrint (Doc)
import qualified Data.IntMap as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Language.Haskell.Interpreter as H
import qualified Language.Haskell.TH.Syntax as S
import qualified Text.Parsec.Error as PE


-- | The Atomo VM. A Continuation monad wrapping a State monad.
type VM = ContT Value (StateT Env IO)

-- | All values usable in Atomo.
data Value
    -- | A block of expressions, bound to a context and with optional arguments.
    = Block !Value [Pattern] [Expr]

    -- | A boolean value.
    | Boolean { fromBoolean :: !Bool }

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

    -- | An arbitrary grouping of Values.
    | Tuple VVector

    -- | A message value.
    | Message { fromMessage :: Message Value }

    -- | A method value.
    | Method { fromMethod :: Method }

    -- | A particle value.
    | Particle { fromParticle :: Particle Value }

    -- | A process; a communications channel and the thread's ID.
    | Process Channel ThreadId

    -- | A pattern value.
    | Pattern { fromPattern :: Pattern }

    -- | A rational value.
    | Rational Rational

    -- | An object reference.
    | Object
        { oDelegates :: !Delegates
        , oMethods :: !Methods
        }

    -- | A string value; Data.Text.Text.
    | String { fromString :: !T.Text }
    deriving (Show, Typeable)

-- | Methods: responders, slots, and macro.
data Method
    -- | Responds to a message by evaluating an expression with the method's
    -- context available.
    = Responder
        { mPattern :: !(Message Pattern)
        , mContext :: !Value
        , mExpr :: !Expr
        }

    -- | Responds to a macro message by evaluating an expression.
    | Macro
        { mPattern :: !(Message Pattern)
        , mExpr :: !Expr
        }

    -- | Responds to a message by returning a value.
    | Slot
        { mPattern :: !(Message Pattern)
        , mValue :: !Value
        }
    deriving (Eq, Show, Typeable)

-- | Messages sent to objects.
data Message v
    -- | A keyword-delimited message with multiple targets.
    = Keyword
        { mID :: !Int
        , mNames :: [String]
        , mTargets :: [v]
        , mOptionals :: ![Option v]
        }

    -- | A single message sent to one target.
    | Single
        { mID :: !Int
        , mName :: String
        , mTarget :: v
        , mOptionals :: ![Option v]
        }
    deriving (Eq, Show, Typeable)

-- | A named optional value.
data Option v = Option !Int String v
    deriving (Eq, Show, Typeable)

-- | Partial messages.
type Particle v = Message (Maybe v)

-- | Shortcut error values.
data AtomoError
    = Error Value
    | ParseError ParseError
    | DidNotUnderstand (Message Value)
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
--
-- The Eq instance only checks equivalence. For example, named pattern matches
-- only match their patterns, not the names.
data Pattern
    -- | Matches any value.
    = PAny

    -- | Matches a non-empty list, with the given patterns for its head and
    -- tail.
    | PHeadTail Pattern Pattern

    -- | Matches a list of a given length, also pattern-matching its contents.
    | PList [Pattern]

    -- | Matches a tuple of a given length, also pattern-matching its contents.
    | PTuple [Pattern]

    -- | Matches a specific value. When matching objects, the delegates are
    -- checked as well.
    | PMatch Value

    -- | Matches a message value.
    | PMessage (Message Pattern)

    -- | Matches an object delegating to something that matches a pattern.
    | PInstance Pattern

    -- | Matches a value strictly; delegates are not checked.
    | PStrict Pattern

    -- | Matches a value strictly; delegates are not checked.
    | PVariable Pattern

    -- | Gives a name to a pattern; this introduces a binding when
    -- pattern-matching.
    | PNamed String Pattern

    -- | Match an object specified by an expression. When used in a method
    -- definition, it's evaluated and turned into a @PMatch@.
    | PObject Expr

    -- | Structurally matches an expression. The @Expr@ here is an @EQuote@,
    -- inside of which @EUnquote@s have @PNamed@ semantics.
    | PExpr Expr

    -- | Match a keyword particle. @PAny@ matches missing roles, but nothing
    -- else does.
    | PPMKeyword [String] [Pattern]

    -- | Shortcut for the current object we're searching for a method on.
    | PThis

    -- | Matches any @Dispatch@ expression.
    | PEDispatch

    -- | Matches any @Operator@ expression.
    | PEOperator

    -- | Matches any @Primitive@ expression.
    | PEPrimitive

    -- | Matches any @EBlock@ expression.
    | PEBlock

    -- | Matches any @EList@ expression.
    | PEList

    -- | Matches any @ETuple@ expression.
    | PETuple

    -- | Matches any @EMacro@ expression.
    | PEMacro

    -- | Matches any @EParticle@ expression.
    | PEParticle

    -- | Matches any @ETop@ expression.
    | PETop

    -- | Matches any @EQuote@ expression.
    | PEQuote

    -- | Matches any @EUnquote@ expression.
    | PEUnquote

    -- TODO: others
    deriving (Show, Typeable)

-- | Expressions; the nodes in a syntax tree.
data Expr
    = EDefine
        { eLocation :: Maybe SourcePos
        , emPattern :: Message Pattern
        , eExpr :: Expr
        }
    | ESet
        { eLocation :: Maybe SourcePos
        , ePattern :: Pattern
        , eExpr :: Expr
        }
    | EDispatch
        { eLocation :: Maybe SourcePos
        , eMessage :: Message Expr
        }
    | EOperator
        { eLocation :: Maybe SourcePos
        , eNames :: [String]
        , eAssoc :: Assoc
        , ePrec :: Integer
        }
    | EPrimitive
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
    | ETuple
        { eLocation :: Maybe SourcePos
        , eContents :: [Expr]
        }
    | EMacro
        { eLocation :: Maybe SourcePos
        , emPattern :: Message Pattern
        , eExpr :: Expr
        }
    | EForMacro
        { eLocation :: Maybe SourcePos
        , eExpr :: Expr
        }
    | EParticle
        { eLocation :: Maybe SourcePos
        , eParticle :: Particle Expr
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
    | ENewDynamic
        { eLocation :: Maybe SourcePos
        , eBindings :: [(String, Expr)]
        , eExpr :: Expr
        }
    | EDefineDynamic
        { eLocation :: Maybe SourcePos
        , eName :: String
        , eExpr :: Expr
        }
    | ESetDynamic
        { eLocation :: Maybe SourcePos
        , eName :: String
        , eExpr :: Expr
        }
    | EGetDynamic
        { eLocation :: Maybe SourcePos
        , eName :: String
        }
    deriving (Show, Typeable)

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

          -- | The current dynamic environment.
        , dynamic :: DynamicMap

          -- | Unwind actions for call/cc etc.
        , unwinds :: [(Value, Value)]
        }
    deriving Typeable

-- | Operator associativity.
data Assoc = ALeft | ARight
    deriving (Eq, Show, Typeable)

-- | A giant record of the objects for each primitive value.
data IDs =
    IDs
        { idObject :: Value
        , idBlock :: Value
        , idBoolean :: Value
        , idChar :: Value
        , idContinuation :: Value
        , idDouble :: Value
        , idExpression :: Value
        , idHaskell :: Value
        , idInteger :: Value
        , idList :: Value
        , idTuple :: Value
        , idMessage :: Value
        , idMethod :: Value
        , idParticle :: Value
        , idProcess :: Value
        , idPattern :: Value
        , idRational :: Value
        , idString :: Value
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

          -- | Environment that for-macro and macro methods are evaluated in.
        , psEnvironment :: Value
        }
    deriving (Show, Typeable)

-- | A map of operators to their associativity and precedence.
type Operators = [(String, (Assoc, Integer))]

-- | The list of values an object delegates to.
type Delegates = [Value]

-- | An object's methods.
type Methods = IORef (MethodMap, MethodMap)

-- | A channel for sending values through to processes.
type Channel = Chan Value

-- | A selector-indexed and precision-sorted map of methods.
type MethodMap = M.IntMap [Method]

-- | A reference to an object.
{-type ORef = IORef Object-}

-- | A vector containing values.
type VVector = V.Vector Value

-- | A reference to a continuation function.
type Continuation = IORef (Value -> VM Value)

-- | A dynamic environment
type DynamicMap = M.IntMap [Value]


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
    (==) (Tuple a) (Tuple b) = a == b
    (==) (Message a) (Message b) = a == b
    (==) (Method a) (Method b) = a == b
    (==) (Particle a) (Particle b) = a == b
    (==) (Pattern a) (Pattern b) = a == b
    (==) (Process _ a) (Process _ b) = a == b
    (==) (Rational a) (Rational b) = a == b
    (==) (Object _ am) (Object _ bm) = am == bm
    (==) (String a) (String b) = a == b
    (==) _ _ = False

instance Eq Pattern where
    -- check if two patterns are "equivalent", ignoring names for patterns
    -- and other things that mean the same thing
    (==) PAny PAny = True
    (==) (PHeadTail ah at) (PHeadTail bh bt) =
        ah == bh && at == bt
    (==) (PMessage a) (PMessage b) = a == b
    (==) (PList aps) (PList bps) =
        length aps == length bps && and (zipWith (==) aps bps)
    (==) (PMatch a) (PMatch b) = a == b
    (==) (PNamed _ a) (PNamed _ b) = a == b
    (==) (PNamed _ a) b = a == b
    (==) a (PNamed _ b) = a == b
    (==) (PPMKeyword ans aps) (PPMKeyword bns bps) =
        ans == bns && and (zipWith (==) aps bps)
    (==) PThis PThis = True
    (==) _ _ = False


instance Eq Expr where
    (==) (EDefine _ ap' ae) (EDefine _ bp be) = ap' == bp && ae == be
    (==) (ESet _ ap' ae) (ESet _ bp be) = ap' == bp && ae == be
    (==) (EDispatch _ am) (EDispatch _ bm) = am == bm
    (==) (EOperator _ ans aa ap') (EOperator _ bns ba bp) =
        ans == bns && aa == ba && ap' == bp
    (==) (EPrimitive _ a) (EPrimitive _ b) = a == b
    (==) (EBlock _ aas aes) (EBlock _ bas bes) =
        aas == bas && aes == bes
    (==) (EList _ aes) (EList _ bes) = aes == bes
    (==) (EParticle _ ap') (EParticle _ bp) = ap' == bp
    (==) (ETop _) (ETop _) = True
    (==) (EVM {}) (EVM {}) = False
    (==) _ _ = False


instance Show Channel where
    show _ = "Channel"

instance Show Methods where
    show _ = "Methods"

instance Show Continuation where
    show _ = "Continuation"

instance Show (VM a) where
    show _ = "VM"

instance Typeable (VM a) where
    typeOf _ = mkTyConApp (mkTyCon "VM") [typeOf ()]


instance S.Lift Expr where
    lift (EDefine _ p e) = [| EDefine Nothing p e |]
    lift (ESet _ p e) = [| ESet Nothing p e |]
    lift (EDispatch _ m) = [| EDispatch Nothing m |]
    lift (EOperator _ ns a p) = [| EOperator Nothing ns a p |]
    lift (EPrimitive _ v) = [| EPrimitive Nothing v |]
    lift (EBlock _ as es) = [| EBlock Nothing as es |]
    lift (EVM {}) = error "cannot lift EVM"
    lift (EList _ es) = [| EList Nothing es |]
    lift (ETuple _ es) = [| ETuple Nothing es |]
    lift (ETop _) = [| ETop Nothing |]
    lift (EParticle _ p) = [| EParticle Nothing p |]
    lift (EMacro _ p e) = [| EMacro Nothing p e |]
    lift (EForMacro _ e) = [| EForMacro Nothing e |]
    lift (EQuote _ e) = [| EQuote Nothing e |]
    lift (EUnquote _ e) = [| EUnquote Nothing e |]
    lift (ENewDynamic _ bs e) = [| ENewDynamic Nothing bs e |]
    lift (ESetDynamic _ n e) = [| ESetDynamic Nothing n e |]
    lift (EDefineDynamic _ n e) = [| EDefineDynamic Nothing n e |]
    lift (EGetDynamic _ n) = [| EGetDynamic Nothing n |]

instance S.Lift Assoc where
    lift ALeft = [| ALeft |]
    lift ARight = [| ARight |]

instance (S.Lift v) => S.Lift (Message v) where
    lift (Keyword i ns vs os) = [| Keyword i ns vs os |]
    lift (Single i n v os) = [| Single i n v os |]

instance (S.Lift v) => S.Lift (Option v) where
    lift (Option i n v) = [| Option i n v |]

instance S.Lift Value where
    lift (Block s as es) = [| Block s as es |]
    lift (Boolean b) = [| Boolean b |]
    lift (Char c) = [| Char c |]
    lift (Double d) = [| Double $(return $ S.LitE (S.RationalL (toRational d))) |]
    lift (Expression e) = [| Expression e |]
    lift (Integer i) = [| Integer i |]
    lift (Message m) = [| Message m |]
    lift (Particle p) = [| Particle p |]
    lift (Pattern p) = [| Pattern p |]
    lift (String s) = [| String (T.pack $(return $ S.LitE (S.StringL (T.unpack s)))) |]
    lift v = error $ "no lift for: " ++ show v

instance S.Lift Pattern where
    lift PAny = [| PAny |]
    lift (PHeadTail h t) = [| PHeadTail h t |]
    lift (PMessage m) = [| PMessage m |]
    lift (PList ps) = [| PList ps |]
    lift (PTuple ps) = [| PTuple ps |]
    lift (PMatch v) = [| PMatch v |]
    lift (PNamed n p) = [| PNamed n p |]
    lift (PObject e) = [| PObject e |]
    lift (PPMKeyword ns ts) = [| PPMKeyword ns ts |]
    lift PThis = [| PThis |]
    lift PEDispatch = [| PEDispatch |]
    lift PEOperator = [| PEOperator |]
    lift PEPrimitive = [| PEPrimitive |]
    lift PEBlock = [| PEBlock |]
    lift PEList = [| PEList |]
    lift PETuple = [| PETuple |]
    lift PEMacro = [| PEMacro |]
    lift PEParticle = [| PEParticle |]
    lift PETop = [| PETop |]
    lift PEQuote = [| PEQuote |]
    lift PEUnquote = [| PEUnquote |]
    lift (PExpr e) = [| PExpr e |]
    lift (PInstance p) = [| PInstance p |]
    lift (PStrict p) = [| PStrict p |]
    lift (PVariable p) = [| PVariable p |]


-- | Initial "empty" parser state.
startParserState :: ParserState
startParserState = ParserState [] (M.empty, M.empty) False 0 (error "no parser environment")

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
            , idTuple = error "idTuple not set"
            }
    , channel = error "channel not set"
    , halt = error "halt not set"
    , loadPath = []
    , loaded = []
    , stack = []
    , parserState = startParserState
    , dynamic = M.empty
    , unwinds = []
    }

-- | Evaluate x with e as the environment.
runWith :: VM Value -> Env -> IO Value
runWith x = evalStateT (runContT x return)

-- | Evaluate x with e as the environment.
runVM :: VM Value -> Env -> IO (Value, Env)
runVM x = runStateT (runContT x return)


-----------------------------------------------------------------------------
-- Dynamic environment ------------------------------------------------------
-----------------------------------------------------------------------------

newDynamic :: String -> Value -> DynamicMap -> DynamicMap
newDynamic n v m =
    M.insert (hash n) [v] m

bindDynamic :: String -> Value -> DynamicMap -> DynamicMap
bindDynamic n v m =
    M.adjust (v:) (hash n) m

unbindDynamic :: String -> DynamicMap -> DynamicMap
unbindDynamic n m =
    M.adjust tail (hash n) m

setDynamic :: String -> Value -> DynamicMap -> DynamicMap
setDynamic n v m =
    M.adjust ((v:) . tail) (hash n) m

getDynamic :: String -> DynamicMap -> Maybe Value
getDynamic n m = M.lookup (hash n) m >>= listToMaybe

isBound :: String -> DynamicMap -> Bool
isBound n m =
    case M.lookup (hash n) m of
        Just x -> length x > 1
        Nothing -> False


-----------------------------------------------------------------------------
-- Helpers ------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Create a single particle with a given name.
particle :: String -> Value
{-# INLINE particle #-}
particle = Particle . flip single Nothing

-- | Create a keyword particle with a given name and optional values.
keyParticle :: [String] -> [Maybe Value] -> Value
{-# INLINE keyParticle #-}
keyParticle ns vs = Particle $ keyword ns vs

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

-- | Convert a tuple of values into a tuple Value.
tuple :: [Value] -> Value
{-# INLINE tuple #-}
tuple = Tuple . V.fromList

-- | Convert a Text string into a String.
fromText :: T.Text -> String
{-# INLINE fromText #-}
fromText = T.unpack

-- | Convert a List Value into a list of Values.
fromList :: Value -> [Value]
fromList (List vr) = V.toList vr
fromList v = error $ "no fromList for: " ++ show v

-- | Convert a Tuple Value into a list of Values.
fromTuple :: Value -> [Value]
fromTuple (Tuple vr) = V.toList vr
fromTuple v = error $ "no fromTuple for: " ++ show v

-- | Create a single message with a given name and target.
single :: String -> v -> Message v
{-# INLINE single #-}
single n v = Single (hash n) n v []

-- | Create a single message with a given name and target.
single' :: String -> v -> [Option v] -> Message v
{-# INLINE single' #-}
single' n v os = Single (hash n) n v os

-- | Create a keyword message with a given name and targets.
keyword :: [String] -> [v] -> Message v
{-# INLINE keyword #-}
keyword ns vs = Keyword (hash ns) ns vs []

-- | Create a keyword message with a given name and targets.
keyword' :: [String] -> [v] -> [Option v] -> Message v
{-# INLINE keyword' #-}
keyword' ns vs os = Keyword (hash ns) ns vs os

-- | Create an @Option@ with the given name and value.
option :: String -> v -> Option v
{-# INLINE option #-}
option n v = Option (hash n) n v

-- | Create a single message pattern with a given name and target pattern.
psingle :: String -> Pattern -> Pattern
{-# INLINE psingle #-}
psingle n = PMessage . single n

-- | Create a keyword message pattern with a given name and target patterns.
pkeyword :: [String] -> [Pattern] -> Pattern
{-# INLINE pkeyword #-}
pkeyword ns = PMessage . keyword ns

-- | Is a value a `Block'?
isBlock :: Value -> Bool
isBlock (Block _ _ _) = True
isBlock _ = False

-- | Is a value a `Boolean'?
isBoolean :: Value -> Bool
isBoolean (Boolean _) = True
isBoolean _ = False

-- | Is a value a `Char'?
isChar :: Value -> Bool
isChar (Char _) = True
isChar _ = False

-- | Is a value a `Continuation'?
isContinuation :: Value -> Bool
isContinuation (Continuation _) = True
isContinuation _ = False

-- | Is a value a `Double'?
isDouble :: Value -> Bool
isDouble (Double _) = True
isDouble _ = False

-- | Is a value an `Expression'?
isExpression :: Value -> Bool
isExpression (Expression _) = True
isExpression _ = False

-- | Is a value a `Haskell'?
isHaskell :: Value -> Bool
isHaskell (Haskell _) = True
isHaskell _ = False

-- | Is a value an `Integer'?
isInteger :: Value -> Bool
isInteger (Integer _) = True
isInteger _ = False

-- | Is a value a `List'?
isList :: Value -> Bool
isList (List _) = True
isList _ = False

-- | Is a value a `Message'?
isMessage :: Value -> Bool
isMessage (Message _) = True
isMessage _ = False

-- | Is a value a `Method'?
isMethod :: Value -> Bool
isMethod (Method _) = True
isMethod _ = False

-- | Is a value a `Particle'?
isParticle :: Value -> Bool
isParticle (Particle _) = True
isParticle _ = False

-- | Is a value a `Pattern'?
isPattern :: Value -> Bool
isPattern (Pattern _) = True
isPattern _ = False

-- | Is a value a `Process'?
isProcess :: Value -> Bool
isProcess (Process _ _) = True
isProcess _ = False

-- | Is a value a `Rational'?
isRational :: Value -> Bool
isRational (Rational _) = True
isRational _ = False

-- | Is a value a `Object'?
isObject :: Value -> Bool
isObject (Object {}) = True
isObject _ = False

-- | Is a value a `String'?
isString :: Value -> Bool
isString (String _) = True
isString _ = False

-- | Is a value a `Tuple'?
isTuple :: Value -> Bool
isTuple (Tuple _) = True
isTuple _ = False


-- | Convert an AtomoError into the Value we want to error with.
asValue :: AtomoError -> Value
asValue (Error v) = v
asValue (ParseError pe) =
    keyParticleN ["parse-error", "at"]
        [ list (map msgValue (PE.errorMessages pe))
        , spValue (PE.errorPos pe)
        ]
  where
    msgValue (PE.SysUnExpect s) = keyParticleN ["unexpected"] [string s]
    msgValue (PE.UnExpect s) = keyParticleN ["unexpected"] [string s]
    msgValue (PE.Expect s) = keyParticleN ["expected"] [string s]
    msgValue (PE.Message s) = string s

    spValue s =
        keyParticleN ["source", "line", "column"]
            [ string (sourceName s)
            , Integer (fromIntegral $ sourceLine s)
            , Integer (fromIntegral $ sourceColumn s)
            ]
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

-- | Given a record of primitive IDs, get the object backing a value.
objectFrom :: IDs -> Value -> Value
{-# INLINE objectFrom #-}
objectFrom _ o@(Object {}) = o
objectFrom ids (Block _ _ _) = idBlock ids
objectFrom ids (Boolean _) = idBoolean ids
objectFrom ids (Char _) = idChar ids
objectFrom ids (Continuation _) = idContinuation ids
objectFrom ids (Double _) = idDouble ids
objectFrom ids (Expression _) = idExpression ids
objectFrom ids (Haskell _) = idHaskell ids
objectFrom ids (Integer _) = idInteger ids
objectFrom ids (List _) = idList ids
objectFrom ids (Message _) = idMessage ids
objectFrom ids (Method _) = idMethod ids
objectFrom ids (Particle _) = idParticle ids
objectFrom ids (Process _ _) = idProcess ids
objectFrom ids (Pattern _) = idPattern ids
objectFrom ids (Rational _) = idRational ids
objectFrom ids (String _) = idString ids
objectFrom ids (Tuple _) = idTuple ids
