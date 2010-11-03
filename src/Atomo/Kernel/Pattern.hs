{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Pattern (load) where

import Atomo
import Atomo.Method


load :: VM ()
load = do
    [$p|(e: Expression) as: Pattern|] =: do
        Expression e <- here "e" >>= findExpression
        p <- toPattern e
        return (Pattern p)

    [$p|(p: Pattern) name|] =: do
        Pattern p <- here "p" >>= findPattern

        case p of
            PNamed n _ -> return (string n)
            PSingle { ppName = n } -> return (string n)
            _ -> raise ["no-name-for"] [Pattern p]

    [$p|(p: Pattern) names|] =: do
        Pattern p <- here "p" >>= findPattern

        case p of
            PKeyword { ppNames = ns } -> return $ list (map string ns)
            _ -> raise ["no-names-for"] [Pattern p]

    [$p|(p: Pattern) target|] =: do
        Pattern p <- here "p" >>= findPattern

        case p of
            PSingle { ppTarget = t } -> return (Pattern t)
            _ -> raise ["no-target-for"] [Pattern p]

    [$p|(p: Pattern) targets|] =: do
        Pattern p <- here "p" >>= findPattern

        case p of
            PKeyword { ppTargets = ts } -> return $ list (map Pattern ts)
            _ -> raise ["no-targets-for"] [Pattern p]

    [$p|(p: Pattern) matches?: v|] =: do
        Pattern p <- here "p" >>= findPattern
        v <- here "v"
        ids <- gets primitives

        if match ids p v
            then do
                obj <- eval [$e|Object clone|]
                o <- newObject $ \o -> o
                    { oDelegates = [obj]
                    , oMethods = (toMethods (bindings' p v), snd (oMethods o))
                    }

                return (keyParticle ["yes"] [Nothing, Just o])
            else return (particle "no")

    [$p|(p: Pattern) set-to: v|] =: do
        Pattern p <- here "p" >>= findPattern
        v <- here "v"
        s <- eval [$e|sender|]
        withTop s (set p v)
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
    toPattern (Dispatch { eMessage = ESingle { emTarget = ETop {}, emName = n } }) =
        return (PNamed n PAny)
    toPattern (EList { eContents = es }) = do
        ps <- mapM toPattern es
        return (PList ps)
    toPattern (EParticle { eParticle = EPMSingle n }) =
        return (PMatch (Particle (PMSingle n)))
    toPattern (EParticle { eParticle = EPMKeyword ns mes }) = do
        ps <- forM mes $ \me ->
            case me of
                Nothing -> return PAny
                Just e -> toPattern e

        return (PPMKeyword ns ps)
    toPattern (EQuote { eExpr = e }) = return (PExpr e)
    toPattern (Primitive { eValue = v }) =
        return (PMatch v)
    toPattern e = raise ["unknown-pattern"] [Expression e]
