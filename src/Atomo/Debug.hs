module Atomo.Debug where

import Debug.Trace
import Text.Show.Pretty


debugging :: Bool
debugging = False

debug :: (Show a, Show b) => b -> a -> a
debug s v
    | debugging = trace (prettyShow s ++ ": " ++ prettyShow v) v
    | otherwise = v

dump :: (Monad m, Show a) => a -> m ()
dump x
    | debugging = out x
    | otherwise = return ()

out :: (Monad m, Show a) => a -> m ()
out x = trace (prettyShow x) (return ())

prettyShow :: Show a => a -> String
prettyShow = ppShow
