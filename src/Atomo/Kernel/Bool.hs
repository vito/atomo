{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Bool (load) where

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = mapM_ eval [$es|
    Bool = Object clone
    True = Object clone
    False = Object clone

    True delegates-to: Bool
    False delegates-to: Bool

    True && True := True
    Bool && Bool := False

    True || Bool := True
    False || (b: Bool) := b

    True not := False
    False not := True

    if: True then: (a: Block) else: Block :=
        a call

    if: False then: Block else: (b: Block) :=
        b call

    when: (b: Bool) do: (action: Block) :=
        if: b then: action in-scope else: { @ok }

    while: (test: Block) do: (action: Block) :=
        when: test call
            do: {
                action scope do: action
                while: test do: action
            }

    True show := "True"
    False show := "False"
|]
