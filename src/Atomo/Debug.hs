module Atomo.Debug where

import Debug.Trace


debugging :: Bool
debugging = False

debug :: (Show a, Show b) => b -> a -> a
debug s v
    | debugging = dout s v
    | otherwise = v

dump :: (Monad m, Show a) => a -> m ()
dump x
    | debugging = out x
    | otherwise = return ()

out :: (Monad m, Show a) => a -> m ()
out x = trace (prettyShow x) (return ())

dout :: (Show a, Show b) => b -> a -> a
dout s v = trace (prettyShow s ++ ": " ++ prettyShow v) v

prettyShow :: Show a => a -> String
prettyShow = show
