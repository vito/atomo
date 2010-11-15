{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Expression (load) where

import Text.PrettyPrint (Doc)

import Atomo
import Atomo.Pretty (pretty)
import Atomo.Parser (macroExpand, parseInput, withParser)


load :: VM ()
load = do
    [$p|`Block new: (es: List)|] =::: [$e|`Block new: es arguments: []|]
    [$p|`Block new: (es: List) arguments: (as: List)|] =: do
        es <- getList [$e|es|]
        as <- getList [$e|as|]
        return (Expression (EBlock Nothing (map fromPattern as) (map fromExpression es)))

    [$p|`List new: (es: List)|] =: do
        es <- getList [$e|es|]
        return (Expression (EList Nothing (map fromExpression es)))

    [$p|`Match new: (branches: List) on: (value: Expression)|] =: do
        pats <- liftM (map fromExpression) $ getList [$e|branches map: @from|]
        exprs <- liftM (map fromExpression) $ getList [$e|branches map: @to|]
        Expression value <- here "value" >>= findExpression

        ps <- mapM toPattern pats
        ids <- gets primitives
        return . Expression . EVM Nothing (Just $ prettyMatch value (zip pats exprs)) $
            eval value >>= matchBranches ids (zip ps exprs)

    [$p|(s: String) parse-expressions|] =:
        getString [$e|s|] >>= liftM (list . map Expression) . parseInput

    [$p|top evaluate: (e: Expression)|] =: do
        t <- here "top"
        Expression e <- here "e" >>= findExpression
        withTop t (eval e)

    [$p|(e: Expression) expand|] =: do
        Expression e <- here "e" >>= findExpression
        liftM Expression $ withParser (macroExpand e)

    [$p|(e: Expression) type|] =: do
        Expression e <- here "e" >>= findExpression
        case e of
            Dispatch { eMessage = EKeyword {} } ->
                return (keyParticleN ["dispatch"] [particle "keyword"])
            Dispatch { eMessage = ESingle {} } ->
                return (keyParticleN ["dispatch"] [particle "single"])

            Define {} -> return (particle "define")
            Set {} -> return (particle "set")
            Operator {} -> return (particle "operator")
            Primitive {} -> return (particle "primitive")
            EBlock {} -> return (particle "block")
            EVM {} -> return (particle "vm")
            EList {} -> return (particle "list")
            EMacro {} -> return (particle "macro")
            ETop {} -> return (particle "top")
            EQuote {} -> return (particle "quote")
            EUnquote {} -> return (particle "unquote")

            EParticle { eParticle = EPMKeyword _ _ } ->
                return (keyParticleN ["particle"] [particle "keyword"])
            EParticle { eParticle = EPMSingle _ } ->
                return (keyParticleN ["particle"] [particle "single"])

    [$p|(e: Expression) target|] =: do
        Expression e <- here "e" >>= findExpression

        case e of
            Dispatch { eMessage = ESingle { emTarget = t } } ->
                return (Expression t)
            _ -> raise ["no-target-for"] [Expression e]

    [$p|(e: Expression) targets|] =: do
        Expression e <- here "e" >>= findExpression

        case e of
            Dispatch { eMessage = EKeyword { emTargets = ts } } ->
                return (list (map Expression ts))
            _ -> raise ["no-targets-for"] [Expression e]

    [$p|(e: Expression) name|] =: do
        Expression e <- here "e" >>= findExpression

        case e of
            EParticle _ (EPMSingle n) -> return (string n)
            Dispatch { eMessage = ESingle { emName = n } } ->
                return (string n)
            _ -> raise ["no-name-for"] [Expression e]

    [$p|(e: Expression) names|] =: do
        Expression e <- here "e" >>= findExpression

        case e of
            EParticle _ (EPMKeyword ns _) ->
                return (list (map string ns))
            Dispatch { eMessage = EKeyword { emNames = ns } } ->
                return (list (map string ns))
            _ -> raise ["no-names-for"] [Expression e]

    [$p|(e: Expression) values|] =: do
        Expression e <- here "e" >>= findExpression

        case e of
            EParticle { eParticle = EPMKeyword _ mes } ->
                return . list $
                    map
                        (maybe (particle "none") (keyParticleN ["ok"] . (:[]) . Expression))
                        mes
            _ -> raise ["no-values-for"] [Expression e]

    [$p|(e: Expression) contents|] =: do
        Expression e <- here "e" >>= findExpression

        case e of
            EBlock { eContents = es } ->
                return (list (map Expression es))
            EList { eContents = es } ->
                return (list (map Expression es))
            _ -> raise ["no-contents-for"] [Expression e]

    [$p|(e: Expression) arguments|] =: do
        Expression e <- here "e" >>= findExpression

        case e of
            EBlock { eArguments = as } ->
                return (list (map Pattern as))
            _ -> raise ["no-arguments-for"] [Expression e]

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
            EQuote { eExpr = e } -> return (Expression e)
            EUnquote { eExpr = e } -> return (Expression e)
            _ -> raise ["no-expression-for"] [Expression e]


matchBranches :: IDs -> [(Pattern, Expr)] -> Value -> VM Value
matchBranches _ [] v = raise ["no-match-for"] [v]
matchBranches ids ((p, e):ps) v = do
    p' <- matchable p
    if match ids p' v
        then newScope $ set p' v >> eval e
        else matchBranches ids ps v

prettyMatch :: Expr -> [(Expr, Expr)] -> Doc
prettyMatch t bs =
    pretty . Dispatch Nothing $
        ekeyword ["match"] [t, EBlock Nothing [] branches]
  where
    branches = flip map bs $ \(p, e) ->
        Dispatch Nothing $ ekeyword ["->"] [p, e]
