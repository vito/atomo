{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Block (load) where

import Atomo


load :: VM ()
load = do
    [$p|Block new: (l: List)|] =:::
        [$e|Block new: l in: sender|]

    [$p|Block new: (l: List) in: t|] =: do
        t <- here "t"
        es <- getList [$e|l|]

        let toExpr (Expression e') = e'
            toExpr v = Primitive Nothing v

        return (Block t [] (map toExpr es))

    [$p|(b: Block) call|] =: do
        b <- here "b" >>= findBlock
        callBlock b []

    [$p|(b: Block) call: (l: List)|] =: do
        b <- here "b" >>= findBlock
        vs <- getList [$e|l|]
        callBlock b vs

    [$p|(b: Block) context|] =: do
        Block s _ _ <- here "b" >>= findBlock
        return s

    [$p|(b: Block) arguments|] =: do
        Block _ as _ <- here "b" >>= findBlock
        return $ list (map Pattern as)

    [$p|(b: Block) contents|] =: do
        Block _ _ es <- here "b" >>= findBlock
        return $ list (map Expression es)
