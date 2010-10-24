{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Association (load) where

import Atomo


load :: VM ()
load = mapM_ eval [$es|
    operator right 1 ->

    Association = Object clone
    a -> b := Association clone do: { from = a; to = b }

    (a: Association) show :=
        a from show .. " -> " .. a to show

    [] lookup: _ = @none
    (a . as) lookup: k :=
      if: (k == a from)
        then: { @(ok: a to) }
        else: { as lookup: k }

    [] find: _ = @none
    (a . as) find: k :=
      if: (k == a from)
        then: { @(ok: a) }
        else: { as find: k }

    [] set: k to: v := [k -> v]
    (a . as) set: k to: v :=
      if: (k == a from)
        then: { (k -> v) . as }
        else: { a . (as set: k to: v) }
|]
