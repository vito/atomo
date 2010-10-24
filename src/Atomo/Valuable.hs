module Atomo.Valuable where

import Control.Monad.Trans (liftIO)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector as V

import Atomo.Types


class Valuable a where
    toValue :: a -> VM Value
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
    toValue xs = mapM toValue xs >>= return . list
    fromValue (List v) = mapM fromValue (V.toList v)

instance Valuable a => Valuable (V.Vector a) where
    toValue xs = V.mapM toValue xs >>= return . List
    fromValue (List v) = V.mapM fromValue v

instance Valuable T.Text where
    toValue = return . String
    fromValue (String s) = return s
