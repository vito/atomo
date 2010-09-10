{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Pattern (load) where

import Atomo.Environment
import Atomo.Haskell
import Atomo.Method


load :: VM ()
load = do
    [$p|(e: Expression) as: Pattern|] =: do
        Expression e <- here "e" >>= findValue isExpression
        p <- toPattern e
        return (Pattern p)

    [$p|(p: Pattern) matches?: v|] =: do
        Pattern p <- here "p" >>= findValue isPattern
        v <- here "v"
        ids <- lift (gets primitives)

        if match ids p v
            then do
                obj <- eval [$e|Object clone|]
                o <- newObject $ \o -> o
                    { oDelegates = [obj]
                    , oMethods = (toMethods (bindings' p v), snd (oMethods o))
                    }

                return (keyParticle ["yes"] [Nothing, Just o])
            else return (particle "no")
  where
    -- convert an expression to the pattern match it represents
    toPattern (Dispatch { eMessage = EKeyword { emNames = ["."], emTargets = [h, t] } }) = do
        hp <- toPattern h
        tp <- toPattern t
        return (PHeadTail hp tp)
    toPattern (Dispatch { eMessage = EKeyword { emNames = [n], emTargets = [ETop {}, x] } }) = do
        p <- toPattern x
        return (PNamed n p)
    toPattern (Dispatch { eMessage = ESingle { emName = "_" } }) =
        return PAny
    toPattern (Dispatch { eMessage = ESingle { emName = n } }) =
        return (PNamed n PAny)
    toPattern (EList { eContents = es }) = do
        ps <- mapM toPattern es
        return (PList ps)
    toPattern (EParticle { eParticle = EPMSingle n }) =
        return (PPMSingle n)
    toPattern (EParticle { eParticle = EPMKeyword ns mes }) = do
        ps <- forM mes $ \me ->
            case me of
                Nothing -> return PAny
                Just e -> toPattern e

        return (PPMKeyword ns ps)
    toPattern (Primitive { eValue = v }) =
        return (PMatch v)
    toPattern e = throwError . ErrorMsg $ "don't know how to convert to pattern: " ++ show (e)
