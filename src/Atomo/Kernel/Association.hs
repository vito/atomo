{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Association (load) where

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = mapM_ eval [$es|
    operator right 1 ->

    Association = Object clone
    a -> b := Association clone do: { from = a; to = b }
    (a: Association) show := a from show .. " -> " .. a to show
|]
