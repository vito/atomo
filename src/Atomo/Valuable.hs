module Atomo.Valuable where

import Control.Monad (liftM)
import qualified Data.Text as T
import qualified Data.Vector as V

import Atomo.Environment
import Atomo.Types


class Valuable a where
    -- | Convert to an Atomo value.
    toValue :: a -> VM Value

    -- | Convert from an Atomo value.
    fromValue :: Value -> VM a

instance Valuable Value where
    toValue = return
    fromValue = return

instance Valuable Char where
    toValue = return . Char
    fromValue (Char c) = return c

instance Valuable Double where
    toValue = return . Double
    fromValue (Double d) = return d

instance Valuable Float where
    toValue = return . Double . fromRational . toRational
    fromValue (Double d) = return (fromRational . toRational $ d)

instance Valuable Integer where
    toValue = return . Integer
    fromValue (Integer i) = return i

instance Valuable Int where
    toValue = return . Integer . fromIntegral
    fromValue (Integer i) = return (fromIntegral i)

instance Valuable a => Valuable [a] where
    toValue xs = liftM list (mapM toValue xs)
    fromValue (List v) = mapM fromValue (V.toList v)

instance Valuable a => Valuable (V.Vector a) where
    toValue xs = liftM List (V.mapM toValue xs)
    fromValue (List v) = V.mapM fromValue v

instance Valuable T.Text where
    toValue = return . String
    fromValue (String s) = return s

instance Valuable x => Valuable (Maybe x) where
    toValue (Just x) = liftM (keyParticleN ["ok"] . (:[])) (toValue x)
    toValue Nothing = return (particle "none")

    fromValue (Particle (Single { mName = "none" })) = return Nothing
    fromValue (Particle (Keyword { mNames = ["ok"], mTargets = [_, Just v]})) =
        liftM Just (fromValue v)

instance (Valuable x, Valuable y) => Valuable (x, y) where
    toValue (x, y) = do
        xv <- toValue x
        yv <- toValue y
        dispatch (keyword ["->"] [xv, yv])

    fromValue v = do
        x <- dispatch (single "from" v) >>= fromValue
        y <- dispatch (single "to" v) >>= fromValue
        return (x, y)
