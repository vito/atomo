{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Pattern (load) where

import Atomo
import Atomo.Pattern (match)


load :: VM ()
load = do
    ([p|Pattern Role|] =::) =<< eval [e|Pattern clone|]
    ([p|Pattern Define|] =::) =<< eval [e|Pattern clone|]

    [p|(e: Expression) to: Pattern|] =: do
        Expression e <- here "e" >>= findExpression
        p <- toPattern' e
        return (Pattern p)

    [p|(e: Expression) to: Pattern Role|] =: do
        Expression e <- here "e" >>= findExpression
        p <- toRolePattern' e
        return (Pattern p)

    [p|(e: Expression) to: Pattern Define|] =: do
        Expression e <- here "e" >>= findExpression
        p <- toDefinePattern' e
        return (Pattern (PMessage p))

    [p|(p: Pattern) name|] =: do
        Pattern p <- here "p" >>= findPattern

        case p of
            PNamed n _ -> return (string n)
            PMessage (Single { mName = n }) -> return (string n)
            _ -> raise ["no-name-for"] [Pattern p]

    [p|(p: Pattern) names|] =: do
        Pattern p <- here "p" >>= findPattern

        case p of
            PMessage (Keyword { mNames = ns }) -> return $ list (map string ns)
            _ -> raise ["no-names-for"] [Pattern p]

    [p|(p: Pattern) target|] =: do
        Pattern p <- here "p" >>= findPattern

        case p of
            PMessage (Single { mTarget = t }) -> return (Pattern t)
            _ -> raise ["no-target-for"] [Pattern p]

    [p|(p: Pattern) targets|] =: do
        Pattern p <- here "p" >>= findPattern

        case p of
            PMessage (Keyword { mTargets = ts }) -> return $ list (map Pattern ts)
            _ -> raise ["no-targets-for"] [Pattern p]

    [p|(p: Pattern) match: v|] =: do
        Pattern p <- here "p" >>= findPattern
        v <- here "v"
        ids <- gets primitives

        if match ids Nothing p v
            then do
                bs <- eval [e|Object clone|]
                withTop bs (set p v)
                return (keyParticleN ["ok"] [bs])
            else return (particle "none")

    [p|top match: (p: Pattern) on: v|] =: do
        p <- here "p" >>= findPattern >>= matchable' . fromPattern
        v <- here "v"
        t <- here "top"

        case p of
            PMessage m -> define m (EPrimitive Nothing v) >> return v
            _          -> withTop t (set p v)
