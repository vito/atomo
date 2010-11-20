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

-- | define a method as an action returning a value
(=:) :: Pattern -> VM Value -> VM ()
pat =: vm = define pat (EVM Nothing Nothing vm)

-- | define a slot to a given value
(=::) :: Pattern -> Value -> VM ()
pat =:: v = define pat (Primitive Nothing v)

-- | define a method that evaluates e
(=:::) :: Pattern -> Expr -> VM ()
pat =::: e = define pat e

-- | find a value from an object, searching its delegates, throwing
-- a descriptive error if it is not found
findValue :: String -> (Value -> Bool) -> Value -> VM Value
findValue _ t v | t v = return v
findValue d t v = findValue' t v >>= maybe die return
  where
    die = throwError (ValueNotFound d v)

-- | findValue, but returning Nothing instead of failing
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

findBlock :: Value -> VM Value
findBlock v
    | isBlock v = return v
    | otherwise = findValue "Block" isBlock v

findBoolean :: Value -> VM Value
findBoolean v
    | isBoolean v = return v
    | otherwise = findValue "Boolean" isBoolean v

findChar :: Value -> VM Value
findChar v
    | isChar v = return v
    | otherwise = findValue "Char" isChar v

findContinuation :: Value -> VM Value
findContinuation v
    | isContinuation v = return v
    | otherwise = findValue "Continuation" isContinuation v

findDouble :: Value -> VM Value
findDouble v
    | isDouble v = return v
    | otherwise = findValue "Double" isDouble v

findExpression :: Value -> VM Value
findExpression v
    | isExpression v = return v
    | otherwise = findValue "Expression" isExpression v

findHaskell :: Value -> VM Value
findHaskell v
    | isHaskell v = return v
    | otherwise = findValue "Haskell" isHaskell v

findInteger :: Value -> VM Value
findInteger v
    | isInteger v = return v
    | otherwise = findValue "Integer" isInteger v

findList :: Value -> VM Value
findList v
    | isList v = return v
    | otherwise = findValue "List" isList v

findMessage :: Value -> VM Value
findMessage v
    | isMessage v = return v
    | otherwise = findValue "Message" isMessage v

findMethod' :: Value -> VM Value
findMethod' v
    | isMethod v = return v
    | otherwise = findValue "Method" isMethod v

findParticle :: Value -> VM Value
findParticle v
    | isParticle v = return v
    | otherwise = findValue "Particle" isParticle v

findProcess :: Value -> VM Value
findProcess v
    | isProcess v = return v
    | otherwise = findValue "Process" isProcess v

findPattern :: Value -> VM Value
findPattern v
    | isPattern v = return v
    | otherwise = findValue "Pattern" isPattern v

findRational :: Value -> VM Value
findRational v
    | isRational v = return v
    | otherwise = findValue "Rational" isRational v

findReference :: Value -> VM Value
findReference v
    | isReference v = return v
    | otherwise = findValue "Reference" isReference v

findString :: Value -> VM Value
findString v
    | isString v = return v
    | otherwise = findValue "String" isString v

getString :: Expr -> VM String
getString e = eval e >>= liftM (fromText . fromString) . findString

getText :: Expr -> VM T.Text
getText e = eval e >>= findString >>= \(String t) -> return t

getList :: Expr -> VM [Value]
getList = liftM V.toList . getVector

getVector :: Expr -> VM (V.Vector Value)
getVector e = eval e
    >>= findList
    >>= \(List v) -> return v

here :: String -> VM Value
here n = gets top >>= dispatch . single n

ifVM :: VM Value -> VM a -> VM a -> VM a
ifVM c a b = do
    r <- c
    if r == Boolean True then a else b

ifVM' :: VM Bool -> VM a -> VM a -> VM a
ifVM' c a b = do
    r <- c
    if r then a else b

ifE :: Expr -> VM a -> VM a -> VM a
ifE = ifVM . eval

referenceTo :: Value -> VM Value
{-# INLINE referenceTo #-}
referenceTo = liftM Reference . orefFor

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

doBlock :: MethodMap -> Value -> [Expr] -> VM Value
{-# INLINE doBlock #-}
doBlock bms s es = do
    blockScope <- newObject $ \o -> o
        { oDelegates = [s]
        , oMethods = (bms, emptyMap)
        }

    withTop blockScope (evalAll es)

objectFor :: Value -> VM Object
{-# INLINE objectFor #-}
objectFor v = orefFor v >>= liftIO . readIORef

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

-- | is one value an instance of, equal to, or a delegation to another?
-- for example, 1 is-a?: Integer, but 1 does not delegates-to?: Integer
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

fromHaskell :: Typeable a => String -> Value -> VM a
fromHaskell t (Haskell d) =
    case fromDynamic d of
        Just a -> return a
        Nothing -> raise ["dynamic-needed"] [string t]
fromHaskell t _ = raise ["dynamic-needed"] [string t]

toPattern' :: Expr -> VM Pattern
toPattern' = tryPattern toPattern

toDefinePattern' :: Expr -> VM Pattern
toDefinePattern' = tryPattern toDefinePattern

toRolePattern' :: Expr -> VM Pattern
toRolePattern' = tryPattern toRolePattern

toMacroPattern' :: Expr -> VM Pattern
toMacroPattern' = tryPattern toMacroPattern

tryPattern :: (Expr -> Maybe Pattern) -> Expr -> VM Pattern
tryPattern c e = 
    case c e of
        Nothing -> raise ["unknown-pattern"] [Expression e]
        Just p -> return p
