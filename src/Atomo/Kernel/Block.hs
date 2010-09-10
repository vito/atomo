{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Block (load) where

import qualified Data.IntMap as M
import qualified Data.Vector as V

import Atomo.Environment
import Atomo.Haskell
import Atomo.Method


load :: VM ()
load = do
    [$p|Block new: (l: List)|] =:::
        [$e|Block new: l in: dispatch sender|]

    [$p|Block new: (l: List) in: t|] =: do
        t <- here "t"
        es <- fmap V.toList $ getList [$e|l|]

        let toExpr (Expression e') = e'
            toExpr v = Primitive Nothing v

        return (Block t [] (map toExpr es))

    [$p|(b: Block) call|] =: do
        Block s as es <- here "b" >>= findValue isBlock

        if length as > 0
            then throwError . ErrorMsg $ "block expects " ++ show (length as) ++ ", given 0"
            else doBlock M.empty s es

    [$p|(b: Block) call: (l: List)|] =: do
        Block s ps es <- here "b" >>= findValue isBlock
        vs <- fmap V.toList $ getList [$e|l|]

        if length ps > length vs
            then throwError . ErrorMsg . unwords $
                [ "block expects"
                , show (length ps)
                , "arguments, given"
                , show (length vs)
                ]
            else doBlock (toMethods . concat $ zipWith bindings' ps vs) s es

    [$p|(b: Block) scope|] =: do
        Block s _ _ <- here "b" >>= findValue isBlock
        return s

    [$p|(b: Block) arguments|] =: do
        Block _ as _ <- here "b" >>= findValue isBlock
        list (map Pattern as)

    [$p|(b: Block) contents|] =: do
        Block _ _ es <- here "b" >>= findValue isBlock
        list (map Expression es)
