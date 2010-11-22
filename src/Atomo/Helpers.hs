{-# LANGUAGE ScopedTypeVariables #-}
module Atomo.Helpers where

import "monads-fd" Control.Monad.State
import Data.Dynamic
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector as V

import Atomo.Environment
import Atomo.Method
import Atomo.Pattern
import Atomo.Types


infixr 0 =:, =::

-- | Define a method as an action returning a value.
(=:) :: Pattern -> VM Value -> VM ()
pat =: vm = define pat (EVM Nothing Nothing vm)

-- | Set a slot to a given value.
(=::) :: Pattern -> Value -> VM ()
pat =:: v = define pat (Primitive Nothing v)

-- | Define a method that evaluates e.
(=:::) :: Pattern -> Expr -> VM ()
pat =::: e = define pat e

-- | Find a value, searching through an object's delegates, and throwing
-- @\@could-not-find:in:@ if it is not found.
findValue :: String -> (Value -> Bool) -> Value -> VM Value
findValue _ t v | t v = return v
findValue d t v = findValue' t v >>= maybe die return
  where
    die = throwError (ValueNotFound d v)

-- | Same as `findValue', but returning Nothing instead of failing
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

-- | `findValue' for `Block'
findBlock :: Value -> VM Value
findBlock v
    | isBlock v = return v
    | otherwise = findValue "Block" isBlock v

-- | `findValue' for `Boolean'
findBoolean :: Value -> VM Value
findBoolean v
    | isBoolean v = return v
    | otherwise = findValue "Boolean" isBoolean v

-- | `findValue' for `Char'
findChar :: Value -> VM Value
findChar v
    | isChar v = return v
    | otherwise = findValue "Char" isChar v

-- | `findValue' for `Continuation'
findContinuation :: Value -> VM Value
findContinuation v
    | isContinuation v = return v
    | otherwise = findValue "Continuation" isContinuation v

-- | `findValue' for `Double'
findDouble :: Value -> VM Value
findDouble v
    | isDouble v = return v
    | otherwise = findValue "Double" isDouble v

-- | `findValue' for `Expression'
findExpression :: Value -> VM Value
findExpression v
    | isExpression v = return v
    | otherwise = findValue "Expression" isExpression v

-- | `findValue' for `Haskell'
findHaskell :: Value -> VM Value
findHaskell v
    | isHaskell v = return v
    | otherwise = findValue "Haskell" isHaskell v

-- | `findValue' for `Integer'
findInteger :: Value -> VM Value
findInteger v
    | isInteger v = return v
    | otherwise = findValue "Integer" isInteger v

-- | `findValue' for `List'
findList :: Value -> VM Value
findList v
    | isList v = return v
    | otherwise = findValue "List" isList v

-- | `findValue' for `Message'
findMessage :: Value -> VM Value
findMessage v
    | isMessage v = return v
    | otherwise = findValue "Message" isMessage v

-- | `findValue' for `Method''
findMethod' :: Value -> VM Value
findMethod' v
    | isMethod v = return v
    | otherwise = findValue "Method" isMethod v

-- | `findValue' for `Particle'
findParticle :: Value -> VM Value
findParticle v
    | isParticle v = return v
    | otherwise = findValue "Particle" isParticle v

-- | `findValue' for `Process'
findProcess :: Value -> VM Value
findProcess v
    | isProcess v = return v
    | otherwise = findValue "Process" isProcess v

-- | `findValue' for `Pattern'
findPattern :: Value -> VM Value
findPattern v
    | isPattern v = return v
    | otherwise = findValue "Pattern" isPattern v

-- | `findValue' for `Rational'
findRational :: Value -> VM Value
findRational v
    | isRational v = return v
    | otherwise = findValue "Rational" isRational v

-- | `findValue' for `Reference'
findReference :: Value -> VM Value
findReference v
    | isReference v = return v
    | otherwise = findValue "Reference" isReference v

-- | `findValue' for `String'
findString :: Value -> VM Value
findString v
    | isString v = return v
    | otherwise = findValue "String" isString v

-- | Find a String given an expression to evaluate.
getString :: Expr -> VM String
getString e = eval e >>= liftM (fromText . fromString) . findString

-- | Find a Data.Text.Text given an expression to evaluate.
getText :: Expr -> VM T.Text
getText e = eval e >>= findString >>= \(String t) -> return t

-- | Find a list of values, given an expression to evaluate.
getList :: Expr -> VM [Value]
getList = liftM V.toList . getVector

-- | Find a VVector, given an expression to evaluate.
getVector :: Expr -> VM VVector
getVector e = eval e
    >>= findList
    >>= \(List v) -> return v

-- | Dispatch a single message to the current toplevel.
here :: String -> VM Value
here n = gets top >>= dispatch . single n

-- | if-then-else based on a VM action yielding a Boolean Value.
ifVM :: VM Value -> VM a -> VM a -> VM a
ifVM c a b = do
    r <- c
    if r == Boolean True then a else b

-- | if-then-else based on a VM action yielding a Bool.
ifVM' :: VM Bool -> VM a -> VM a -> VM a
ifVM' c a b = do
    r <- c
    if r then a else b

-- | if-then-else based on an expression to evaluate.
ifE :: Expr -> VM a -> VM a -> VM a
ifE = ifVM . eval

-- | Get a value's object.
referenceTo :: Value -> VM Value
{-# INLINE referenceTo #-}
referenceTo = liftM Reference . orefFor

-- | Call a block with the given arguments. Creates a scope, checks that its
-- argument patterns match, and executes it with the bindings.
callBlock :: Value -> [Value] -> VM Value
callBlock (Block s ps es) vs = do
    is <- gets primitives
    checkArgs is ps vs
    doBlock (toMethods . concat $ zipWith bindings' ps vs) s es
  where
    checkArgs _ [] _ = return (particle "ok")
    checkArgs _ _ [] = throwError (BlockArity (length ps) (length vs))
    checkArgs is (p:ps') (v:vs')
        | match is Nothing p v = checkArgs is ps' vs'
        | otherwise = throwError (Mismatch p v)
callBlock x _ = raise ["not-a-block"] [x]

-- | Evaluate multiple expressions given a context and bindings for the
-- toplevel object.
doBlock :: MethodMap -> Value -> [Expr] -> VM Value
{-# INLINE doBlock #-}
doBlock bms s es = do
    blockScope <- newObject $ \o -> o
        { oDelegates = [s]
        , oMethods = (bms, emptyMap)
        }

    withTop blockScope (evalAll es)

-- | Get the object backing a value.
objectFor :: Value -> VM Object
{-# INLINE objectFor #-}
objectFor v = orefFor v >>= liftIO . readIORef

-- | Does one value delegate to another?
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

-- | Is one value an instance of, equal to, or a delegation to another?
--
-- For example, 1 is-a?: Integer, but 1 does not delegates-to?: Integer
isA :: Value -> Value -> VM Bool
isA x y = do
    xr <- orefFor x
    yr <- orefFor y

    if xr == yr
        then return True
        else do
            ds <- liftM oDelegates (objectFor x)
            isA' ds
  where
    isA' [] = return False
    isA' (d:ds) = do
        di <- isA d y
        if di
            then return True
            else isA' ds

-- | Attempt conversion from a Haskell Value, given the type we expect as
-- a string.
--
-- If conversion fails, raises @\@dynamic-needed:@ with the given string.
fromHaskell :: forall a. Typeable a => Value -> VM a
fromHaskell (Haskell d) =
    case fromDynamic d of
        Just a -> return a
        Nothing ->
            raise ["dynamic-needed", "got"]
                [ string (show (typeOf (undefined :: a)))
                , string (show (dynTypeRep d))
                ]
fromHaskell u =
    raise ["dynamic-needed", "given"]
        [string (show (typeOf (undefined :: a))), u]

-- | Convert an Atomo Haskell dynamic value into its value, erroring on
-- failure.
fromHaskell' :: forall a. Typeable a => Value -> a
fromHaskell' (Haskell d) =
    case fromDynamic d of
        Just a -> a
        Nothing ->
            error $ unwords
                [ "needed Haskell value of type"
                , show (typeOf (undefined :: a))
                , "but got"
                , show (dynTypeRep d)
                ]
fromHaskell' v =
    error $ unwords
        [ "needed Haskell value of type"
        , show (typeOf (undefined :: a))
        , "but given value"
        , show v
        ]

-- | `toPattern', raising @\@unknown-pattern:@ if conversion fails.
toPattern' :: Expr -> VM Pattern
toPattern' = tryPattern toPattern

-- | `toDefinePattern', raising @\@unknown-pattern:@ if conversion fails.
toDefinePattern' :: Expr -> VM Pattern
toDefinePattern' = tryPattern toDefinePattern

-- | `toRolePattern', raising @\@unknown-pattern:@ if conversion fails.
toRolePattern' :: Expr -> VM Pattern
toRolePattern' = tryPattern toRolePattern

-- | `toMacroPattern', raising @\@unknown-pattern:@ if conversion fails.
toMacroPattern' :: Expr -> VM Pattern
toMacroPattern' = tryPattern toMacroPattern

-- | Try a given pattern conversion, raising @\@unknown-pattern:@ if conversion
-- fails.
tryPattern :: (Expr -> Maybe Pattern) -> Expr -> VM Pattern
tryPattern c e = 
    case c e of
        Nothing -> raise ["unknown-pattern"] [Expression e]
        Just p -> return p

-- | Fill in the empty values of a particle. The number of values missing
-- is expected to be equal to the number of values provided.
completeKP :: [Maybe Value] -> [Value] -> [Value]
completeKP [] _ = []
completeKP (Nothing:mvs') (v:vs') = v : completeKP mvs' vs'
completeKP (Just v:mvs') vs' = v : completeKP mvs' vs'
completeKP mvs' vs' = error $ "impossible: completeKP on " ++ show (mvs', vs')
