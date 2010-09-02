{-# LANGUAGE OverloadedStrings #-}

import The.Haskell

load = do
    o <- exec "Object clone"
    "Expression" =: return o
    "Expression from: (b: Block)" =: do
        Block _ _ (e:_) <- getSymbol "b"
        return (Expression e)

    "Block new: (es: List)" =: do
        List es <- getSymbol "es"

        let toExpr (Expression e) = e
            toExpr x = Primitive x

        return (Expression $ EBlock [] (map toExpr es))
