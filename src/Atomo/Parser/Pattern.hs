module Atomo.Parser.Pattern where

import Control.Monad (liftM)
import Text.Parsec

import Atomo.Debug
import Atomo.Parser
import Atomo.Parser.Base
import Atomo.Parser.Primitive
import Atomo.Types hiding (keyword)

pPattern :: Parser Pattern
pPattern = choice
    [ try ppNamed
    , try ppHeadTail
    , try ppInstance
    , try ppStrict
    , try ppMatch
    , ppList
    , ppParticle
    , ppExpr
    , ppAny
    , parens pPattern
    ]

pObjectPattern :: Parser Pattern
pObjectPattern = choice
    [ try ppNamedSensitive
    , try ppHeadTail
    , try ppInstance
    , try ppStrict
    , try ppMatch
    , try ppObject
    , ppList
    , ppParticle
    , ppExpr
    , ppAnySensitive
    , parens pObjectPattern
    ]

ppMacro :: Parser Pattern
ppMacro = try ppMacroKeywords <|> ppMacroSingle

ppInstance :: Parser Pattern
ppInstance = do
    symbol "->"
    pat <- pObjectPattern
    return (PInstance pat)

ppStrict :: Parser Pattern
ppStrict = do
    symbol "=="
    pat <- pObjectPattern
    return (PStrict pat)

ppExpr :: Parser Pattern
ppExpr = do
    q <- pQuasiQuoted
    return (PExpr (eExpr q))

ppMacroSingle :: Parser Pattern
ppMacroSingle = do
    (t, v) <- choice
        [ try $ do
            t <- ppMacroRole
            v <- identifier
            return (t, v)

        , do
            v <- identifier
            return (PAny, v)
        ]

    return $ psingle v t

ppMacroKeywords :: Parser Pattern
ppMacroKeywords = keywords pkeyword PAny ppMacroRole

ppMacroRole :: Parser Pattern
ppMacroRole = choice
    [ ppExpr
    , try $ symbol "Define" >> return PEDefine
    , try $ symbol "Set" >> return PESet
    , try $ symbol "Dispatch" >> return PEDispatch
    , try $ symbol "Operator" >> return PEOperator
    , try $ symbol "Primitive" >> return PEPrimitive
    , try $ symbol "Block" >> return PEBlock
    , try $ symbol "List" >> return PEList
    , try $ symbol "Macro" >> return PEMacro
    , try $ symbol "Particle" >> return PEParticle
    , try $ symbol "Top" >> return PETop
    , try $ symbol "Quote" >> return PEQuote
    , try $ symbol "Unquote" >> return PEUnquote
    , ppAny
    , ppNamedMacro
    ]
  where
    ppNamedMacro = parens $ do
        n <- identifier
        delimit ":"
        p <- ppMacroRole
        return $ PNamed n p

ppNamed :: Parser Pattern
ppNamed = parens $ do
    name <- identifier
    delimit ":"
    p <- pPattern
    return $ PNamed name p

ppNamedSensitive :: Parser Pattern
ppNamedSensitive = parens $ do
    name <- lowIdentifier
    dump ("got ppNamedSensitive", name)
    delimit ":"
    p <- pObjectPattern
    dump ("finished ppNamedSensitive", name, p)
    return $ PNamed name p

ppObject :: Parser Pattern
ppObject = do
    p <- parens pExpr <|> capitalizedCascade
    return $ PObject p
  where
    capitalizedCascade = do
        lookAhead capIdentifier
        pdCascade

ppAny :: Parser Pattern
ppAny = ppWildcard
    <|> ppNamedAny
  where
    ppNamedAny = do
        name <- identifier
        return (PNamed name PAny)

ppAnySensitive :: Parser Pattern
ppAnySensitive = ppWildcard
    <|> ppNamedAny
  where
    ppNamedAny = do
        name <- lowIdentifier
        return (PNamed name PAny)

ppWildcard :: Parser Pattern
ppWildcard = char '_' >> notFollowedBy identifier >> spacing >> return PAny

ppMatch :: Parser Pattern
ppMatch = do
    v <- pPrim
    return $ PMatch v

ppHeadTail :: Parser Pattern
ppHeadTail = parens $ do
    h <- pPattern
    dot
    t <- pPattern
    return $ PHeadTail h t

ppList :: Parser Pattern
ppList = brackets $ do
    ps <- commaSep pPattern
    return $ PList ps

ppParticle :: Parser Pattern
ppParticle = do
    char '@'
    try keywordParticle <|> singleParticle
  where
    singleParticle = liftM (PMatch . Particle . PMSingle) anyIdentifier

    keywordParticle = choice
        [ parens $ do
            ks <- many1 (keyword pPattern)
            let (ns, ps) = unzip ks
            return $ PPMKeyword ns (PAny:ps)
        , do
            o <- operator
            spacing
            return $ PPMKeyword [o] [PAny, PAny]
        , do
            names <- many1 (anyIdent >>= \n -> char ':' >> return n)
            spacing
            return $ PPMKeyword names (replicate (length names + 1) PAny) 
        ]
