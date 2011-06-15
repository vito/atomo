{-# LANGUAGE QuasiQuotes, TypeSynonymInstances #-}
module Atomo.Valuable where

import Control.Monad (liftM)
import System.IO
import qualified Data.Text as T
import qualified Data.Vector as V

import Atomo.Environment
import Atomo.Helpers
import Atomo.Pretty (Prettied)
import Atomo.QuasiQuotes
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
    toValue = return . Character
    fromValue (Character c) = return c
    fromValue v = raise ["wrong-value", "needed"] [v, string "Character"]

instance Valuable Double where
    toValue = return . Double
    fromValue (Double d) = return d
    fromValue v = raise ["wrong-value", "needed"] [v, string "Double"]

instance Valuable Float where
    toValue = return . Double . fromRational . toRational
    fromValue (Double d) = return (fromRational . toRational $ d)
    fromValue v = raise ["wrong-value", "needed"] [v, string "Double"]

instance Valuable Integer where
    toValue = return . Integer
    fromValue (Integer i) = return i
    fromValue v = raise ["wrong-value", "needed"] [v, string "Integer"]

instance Valuable Int where
    toValue = return . Integer . fromIntegral
    fromValue (Integer i) = return (fromIntegral i)
    fromValue v = raise ["wrong-value", "needed"] [v, string "Integer"]

instance Valuable a => Valuable [a] where
    toValue xs = liftM list (mapM toValue xs)
    fromValue (List v) = mapM fromValue (V.toList v)
    fromValue v = raise ["wrong-value", "needed"] [v, string "List"]

instance Valuable a => Valuable (V.Vector a) where
    toValue xs = liftM List (V.mapM toValue xs)
    fromValue (List v) = V.mapM fromValue v
    fromValue v = raise ["wrong-value", "needed"] [v, string "List"]

instance Valuable T.Text where
    toValue = return . String
    fromValue (String s) = return s
    fromValue v = raise ["wrong-value", "needed"] [v, string "String"]

instance Valuable Pattern where
    toValue = return . Pattern
    fromValue (Pattern x) = return x
    fromValue v = raise ["wrong-value", "needed"] [v, string "Pattern"]

instance Valuable Expr where
    toValue = return . Expression
    fromValue (Expression x) = return x
    fromValue v = raise ["wrong-value", "needed"] [v, string "Expression"]

instance Valuable x => Valuable (Maybe x) where
    toValue (Just x) = liftM (keyParticleN ["ok"] . (:[])) (toValue x)
    toValue Nothing = return (particle "none")

    fromValue (Particle (Single { mName = "none" })) = return Nothing
    fromValue (Particle (Keyword { mNames = ["ok"], mTargets = [_, Just v]})) =
        liftM Just (fromValue v)
    fromValue v = raise ["wrong-value", "needed"]
        [ v
         , keyParticleN ["one-of"]
            [ tuple
                [ particle "none"
                , keyParticle ["ok"] [Nothing, Nothing]
                ]
            ]
        ]

instance (Valuable x, Valuable y) => Valuable (x, y) where
    toValue (x, y) = do
        xv <- toValue x
        yv <- toValue y
        dispatch (keyword ["->"] [xv, yv])

    fromValue v = do
        x <- dispatch (single "from" v) >>= fromValue
        y <- dispatch (single "to" v) >>= fromValue
        return (x, y)

instance Valuable BufferMode where
    toValue (BlockBuffering Nothing) = return (particle "block")
    toValue (BlockBuffering (Just i)) = return (keyParticleN ["block"] [Integer (fromIntegral i)])
    toValue LineBuffering = return (particle "line")
    toValue NoBuffering = return (particle "none")

    fromValue (Particle (Single { mName = "block" })) = return (BlockBuffering Nothing)
    fromValue (Particle (Keyword { mNames = ["block"], mTargets = [Nothing, Just (Integer i)] })) =
        return (BlockBuffering (Just (fromIntegral i)))
    fromValue (Particle (Single { mName = "line" })) = return LineBuffering
    fromValue (Particle (Single { mName = "none" })) = return NoBuffering
    fromValue v = raise ["wrong-value", "needed"]
        [ v
         , keyParticleN ["one-of"]
            [ tuple
                [ particle "block"
                , keyParticle ["block"] [Nothing, Nothing]
                , particle "line"
                , particle "none"
                ]
            ]
        ]

instance Valuable Prettied where
    toValue d =
        [e|Pretty|] `newWith` [("doc", haskell d)]

    fromValue v = dispatch (single "doc" v) >>= fromHaskell
