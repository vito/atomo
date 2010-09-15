{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Parameter (load) where

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = mapM_ eval [$es|
    operator right 0 =!

    Parameter = Object clone
    Parameter new: v := Parameter clone do: {
        set-default: v
    }

    (p: Parameter) _? := p value: self

    (p: Parameter) =! v :=
        (p) value: (self) = v

    (p: Parameter) set-default: v :=
        (p) value: _ = v

    with: (p: Parameter) as: new do: (action: Block) := {
        old = p _?
        p =! new
        p ensuring: @(=! old) do: action
    } call

    with-default: (p: Parameter) as: new do: (action: Block) := {
        old = p _?
        p set-default: new
        p ensuring: @(set-default: old) do: action
    } call

    with: [] do: (action: Block) := action call
    with: (b . bs) do: (action: Block) :=
        with: b from as: b to do: { with: bs do: action }

    with-defaults: [] do: (action: Block) := action call
    with-defaults: (b . bs) do: (action: Block) :=
        with-default: b from as: b to do: { with: bs do: action }
|]
