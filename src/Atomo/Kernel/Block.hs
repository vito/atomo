{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Block (load) where

import qualified Data.IntMap as M

import Atomo.Environment
import Atomo.Haskell
import Atomo.Method


load :: VM ()
load = do
    [$p|Block new: (l: List)|] =:::
        [$e|Block new: l in: dispatch sender|]

    [$p|Block new: (l: List) in: t|] =: do
        t <- here "t"
        es <- getList [$e|l|]

        let toExpr (Expression e') = e'
            toExpr v = Primitive Nothing v

        return (Block t [] (map toExpr es))

    [$p|(b: Block) call|] =: do
        Block s as es <- here "b" >>= findBlock

        if length as > 0
            then throwError (BlockArity (length as) 0)
            else doBlock M.empty s es

    [$p|(b: Block) call: (l: List)|] =: do
        Block s ps es <- here "b" >>= findBlock
        vs <- getList [$e|l|]

        if length ps > length vs
            then throwError (BlockArity (length ps) (length vs))
            else doBlock (toMethods . concat $ zipWith bindings' ps vs) s es

    [$p|(b: Block) context|] =: do
        Block s _ _ <- here "b" >>= findBlock
        return s

    [$p|(b: Block) arguments|] =: do
        Block _ as _ <- here "b" >>= findBlock
        list (map Pattern as)

    [$p|(b: Block) contents|] =: do
        Block _ _ es <- here "b" >>= findBlock
        list (map Expression es)

    prelude


prelude :: VM ()
prelude = mapM_ eval [$es|
    (b: Block) repeat := { b in-context call; b repeat } call

    (b: Block) in-context :=
        Object clone do: {
            delegates-to: b
            call := b context join: b
            call: vs := b context join: b with: vs
        }

    (a: Block) .. (b: Block) :=
        Block new: (a contents .. b contents) in: dispatch sender

    (start: Integer) to: (end: Integer) by: (diff: Integer) do: b :=
        (start to: end by: diff) each: b

    (start: Integer) up-to: (end: Integer) do: b :=
        start to: end by: 1 do: b

    (start: Integer) down-to: (end: Integer) do: b :=
        start to: end by: -1 do: b

    (n: Integer) times: (b: Block) :=
        1 up-to: n do: b in-context
|]
