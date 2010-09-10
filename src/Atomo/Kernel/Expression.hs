{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Expression (load) where

import Atomo.Environment
import Atomo.Haskell
import Atomo.Pretty


load :: VM ()
load = do
    [$p|(e: Expression) evaluate|] =: do
        Expression e <- here "e" >>= findValue isExpression
        eval e

    [$p|(e: Expression) type|] =: do
        Expression e <- here "e" >>= findValue isExpression
        case e of
            Dispatch {} -> return (particle "dispatch")
            Define {} -> return (particle "define")
            Set {} -> return (particle "set")
            Operator {} -> return (particle "operator")
            Primitive {} -> return (particle "primitive")
            EBlock {} -> return (particle "block")
            EDispatchObject {} -> return (particle "call")
            EVM {} -> return (particle "vm")
            EList {} -> return (particle "list")
            ETop {} -> return (particle "top")
            EParticle {} -> return (particle "particle")

    [$p|(e: Expression) dispatch-type|] =: do
        Expression (Dispatch _ d) <- here "e" >>= findValue isExpression
        case d of
            ESingle {} -> return (particle "single")
            EKeyword {} -> return (particle "keyword")

    [$p|(e: Expression) particle-type|] =: do
        Expression (EParticle _ p) <- here "e" >>= findValue isExpression
        case p of
            EPMKeyword {} -> return (particle "keyword")
            EPMSingle {} -> return (particle "single")

    [$p|(e: Expression) target|] =: do
        Expression (Dispatch _ (ESingle { emTarget = t })) <- here "e" >>= findValue isExpression
        return (Expression t)

    [$p|(e: Expression) particle|] =: do
        Expression (Dispatch _ em) <- here "e" >>= findValue isExpression

        case em of
            EKeyword { emNames = ns } ->
                return (keyParticle ns (replicate (length ns + 1) Nothing))
            ESingle { emName = n } -> return (particle n)

    [$p|(e: Expression) targets|] =: do
        Expression (Dispatch _ (EKeyword { emTargets = vs })) <- here "e" >>= findValue isExpression
        list (map Expression vs)

    [$p|(e: Expression) name|] =: do
        Expression (EParticle _ (EPMSingle n)) <- here "e" >>= findValue isExpression
        string n

    [$p|(e: Expression) names|] =: do
        Expression (EParticle _ (EPMKeyword ns _)) <- here "e" >>= findValue isExpression
        mapM string ns >>= list

    [$p|(e: Expression) values|] =: do
        Expression (EParticle _ (EPMKeyword _ mes)) <- here "e" >>= findValue isExpression
        list $
            map
                (maybe (particle "none") (keyParticle ["ok"] . ([Nothing] ++) . (:[]). Just . Expression))
                mes

    [$p|(e: Expression) contents|] =: do
        Expression (EList _ es) <- here "e" >>= findValue isExpression
        list (map Expression es)

    [$p|(e: Expression) pattern|] =: do
        Expression e <- here "e" >>= findValue isExpression
        case e of
            Set { ePattern = p } -> return (Pattern p)
            Define { ePattern = p } -> return (Pattern p)
            _ -> throwError $ ErrorMsg $ "no @pattern for " ++ show (pretty e)

    [$p|(e: Expression) expression|] =: do
        Expression e <- here "e" >>= findValue isExpression
        case e of
            Set { eExpr = e } -> return (Expression e)
            Define { eExpr = e } -> return (Expression e)
            _ -> throwError $ ErrorMsg $ "no @expression for " ++ show (pretty (Expression e))
