{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Block (load) where

import Atomo


load :: VM ()
load = do
    [$p|Block new: (es: List) in: t|] =:::
        [$e|Block new: es arguments: [] in: t|]

    [$p|Block new: (es: List) arguments: (as: List) in: t|] =: do
        t <- here "t"
        es <- getList [$e|es|]
        as <- getList [$e|as|]

        return (Block t (map fromPattern as) (map fromExpression es))

    [$p|(b: Block) call|] =: do
        b <- here "b" >>= findBlock
        callBlock b []

    [$p|(b: Block) repeat|] =: do
        b@(Block c _ _) <- here "b" >>= findBlock
        withTop c (forever (callBlock b []))

    [$p|(b: Block) call: (l: List)|] =: do
        b <- here "b" >>= findBlock
        vs <- getList [$e|l|]
        callBlock b vs

    [$p|(b: Block) call-in: c|] =: do
        Block _ _ es <- here "b" >>= findBlock
        c <- here "c"
        withTop c (evalAll es)

    [$p|(b: Block) context|] =: do
        Block s _ _ <- here "b" >>= findBlock
        return s

    [$p|(b: Block) arguments|] =: do
        Block _ as _ <- here "b" >>= findBlock
        return $ list (map Pattern as)

    [$p|(b: Block) contents|] =: do
        Block _ _ es <- here "b" >>= findBlock
        return $ list (map Expression es)
