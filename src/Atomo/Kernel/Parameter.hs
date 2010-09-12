{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Parameter (load) where

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = mapM_ eval [$es|
    Parameter = Object clone
    Parameter new: v := Parameter clone do: {
        value: (self) = v
        value: _ = v
    }

    (p: Parameter) _? := p value: self

    (p: Parameter) set: v :=
        (p) value: (self) = v

    with: (p: Parameter) as: new do: (action: Block) := {
        old = p _?
        p set: new
        p ensuring: @(set: old) do: action
    } call
|]
