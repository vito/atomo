{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Pattern (load) where

import Atomo


load :: VM ()
load = do
    [$p|(e: Expression) as: Pattern|] =: do
        Expression e <- here "e" >>= findExpression
        p <- toPattern' e
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

        if match ids Nothing p v
            then do
                bs <- eval [$e|Object clone|]
                withTop bs (set p v)
                return (keyParticle ["yes"] [Nothing, Just bs])
            else return (particle "no")

    [$p|top match: (p: Pattern) on: v|] =: do
        p <- here "p" >>= findPattern >>= matchable . fromPattern
        v <- here "v"
        t <- here "top"

        let isMethod (PSingle {}) = True
            isMethod (PKeyword {}) = True
            isMethod _ = False

        if isMethod p
            then define p (Primitive Nothing v) >> return v
            else withTop t (set p v)
