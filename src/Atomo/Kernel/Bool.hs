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

    True && True = True
    Bool && Bool = False

    False and: _ = False
    True and: (b: Block) := b call

    True || Bool = True
    False || (b: Bool) := b

    True or: _ = True
    False or: (b: Block) := b call

    True not := False
    False not := True

    if: True then: (a: Block) else: Block :=
        a call

    if: False then: Block else: (b: Block) :=
        b call

    when: (b: Bool) do: (action: Block) :=
        if: b then: { action in-context call; @ok } else: { @ok }

    while: (test: Block) do: (action: Block) :=
        when: test call
            do: {
                action in-context call
                while: test do: action
            }

    True show := "True"
    False show := "False"

    otherwise := True

    condition: (b: Block) :=
        if: b contents empty?
            then: { raise: @no-true-branches }
            else: {
                es = b contents
                [c, e] = es head targets

                if: (c evaluate-in: b context)
                    then: { e evaluate-in: b context }
                    else: { condition: (Block new: es tail in: b context) }
            }
|]
