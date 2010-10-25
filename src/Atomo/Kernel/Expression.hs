{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Expression (load) where

import Atomo
import Atomo.Parser (macroExpand, withParser)


load :: VM ()
load = do
    [$p|(e: Expression) evaluate|] =:::
        [$e|e evaluate-in: dispatch sender|]

    [$p|(e: Expression) evaluate-in: t|] =: do
        Expression e <- here "e" >>= findExpression
        t <- here "t"
        withTop t (eval e)

    [$p|(e: Expression) expand|] =: do
        Expression e <- here "e" >>= findExpression
        liftM Expression $ withParser (macroExpand e)

    [$p|(e: Expression) type|] =: do
        Expression e <- here "e" >>= findExpression
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
        Expression (Dispatch _ d) <- here "e" >>= findExpression
        case d of
            ESingle {} -> return (particle "single")
            EKeyword {} -> return (particle "keyword")

    [$p|(e: Expression) particle-type|] =: do
        Expression (EParticle _ p) <- here "e" >>= findExpression
        case p of
            EPMKeyword {} -> return (particle "keyword")
            EPMSingle {} -> return (particle "single")

    [$p|(e: Expression) target|] =: do
        Expression (Dispatch _ (ESingle { emTarget = t })) <- here "e" >>= findExpression
        return (Expression t)

    [$p|(e: Expression) particle|] =: do
        Expression (Dispatch _ em) <- here "e" >>= findExpression

        case em of
            EKeyword { emNames = ns } ->
                return (keyParticle ns (replicate (length ns + 1) Nothing))
            ESingle { emName = n } -> return (particle n)

    [$p|(e: Expression) targets|] =: do
        Expression (Dispatch _ (EKeyword { emTargets = vs })) <- here "e" >>= findExpression
        return $ list (map Expression vs)

    [$p|(e: Expression) name|] =: do
        Expression (EParticle _ (EPMSingle n)) <- here "e" >>= findExpression
        return (string n)

    [$p|(e: Expression) names|] =: do
        Expression (EParticle _ (EPMKeyword ns _)) <- here "e" >>= findExpression
        return $ list (map string ns)

    [$p|(e: Expression) values|] =: do
        Expression (EParticle _ (EPMKeyword _ mes)) <- here "e" >>= findExpression
        return . list $
            map
                (maybe (particle "none") (keyParticle ["ok"] . ([Nothing] ++) . (:[]). Just . Expression))
                mes

    [$p|(e: Expression) contents|] =: do
        Expression e <- here "e" >>= findExpression
        return $ list (map Expression (eContents e))

    [$p|(e: Expression) pattern|] =: do
        Expression e <- here "e" >>= findExpression
        case e of
            Set { ePattern = p } -> return (Pattern p)
            Define { ePattern = p } -> return (Pattern p)
            _ -> raise ["no-pattern-for"] [Expression e]

    [$p|(e: Expression) expression|] =: do
        Expression e <- here "e" >>= findExpression
        case e of
            Set { eExpr = e } -> return (Expression e)
            Define { eExpr = e } -> return (Expression e)
            _ -> raise ["no-expression-for"] [Expression e]
