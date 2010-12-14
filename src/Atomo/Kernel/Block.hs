{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Block (load) where

import Atomo
import Atomo.Method
import Atomo.Pattern (bindings')


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

    [$p|v do: (b: Block)|] =: do
        v <- here "v"
        b <- here "b" >>= findBlock
        joinWith v b []
        return v
    [$p|v do: (b: Block) with: (l: List)|] =: do
        v <- here "v"
        b <- here "b" >>= findBlock
        as <- getList [$e|l|]
        joinWith v b as
        return v

    [$p|v join: (b: Block)|] =: do
        v <- here "v"
        b <- here "b" >>= findBlock
        joinWith v b []
    [$p|v join: (b: Block) with: (l: List)|] =: do
        v <- here "v"
        b <- here "b" >>= findBlock
        as <- getList [$e|l|]
        joinWith v b as


joinWith :: Value -> Value -> [Value] -> VM Value
joinWith t (Block s ps bes) as
    | length ps > length as =
        throwError (BlockArity (length ps) (length as))

    | null as || null ps =
        case t of
            o@(Object { oDelegates = ds }) ->
                withTop (o { oDelegates = ds ++ [s] }) (evalAll bes)

            _ -> do
                blockScope <- newObject [t, s] noMethods
                withTop blockScope (evalAll bes)

    | otherwise = do
        -- argument bindings
        args <- newObject []
            ( toMethods . concat $ zipWith bindings' ps as
            , emptyMap
            )

        case t of
            o@(Object { oDelegates = ds }) ->
                withTop (o { oDelegates = args : ds ++ [s] })
                    (evalAll bes)

            _ -> do
                blockScope <- newObject [args, t, s] noMethods
                withTop blockScope (evalAll bes)
joinWith _ v _ = error $ "impossible: joinWith on " ++ show v
