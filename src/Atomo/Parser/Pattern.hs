module Atomo.Parser.Pattern where

import Data.Hashable (hash)
import Text.Parsec
import Text.Parsec.String

import Atomo.Debug
import Atomo.Parser
import Atomo.Parser.Base
import Atomo.Parser.Primitive
import Atomo.Types


pPattern :: Parser Pattern
pPattern = choice
    [ try ppNamed
    , try ppHeadTail
    , try ppMatch
    , ppList
    , ppString
    , ppParticle
    , ppAny
    , parens pPattern
    ]

pObjectPattern :: Parser Pattern
pObjectPattern = choice
    [ try ppNamedSensitive
    , try ppHeadTail
    , try ppObject
    , try ppMatch
    , ppList
    , ppString
    , ppParticle
    , ppAnySensitive
    , parens pObjectPattern
    ]

ppDefine :: Parser Pattern
ppDefine = try ppKeywords <|> ppSingle

ppSingle :: Parser Pattern
ppSingle = do
    (t, v) <- choice
        [ try $ do
            t <- pNonExpr
            dump ("got pNonExpr", t)
            v <- identifier
            return (t, v)
        , try $ do
            ds <- pdCascade
            p <- cInit ds
            v <- cLast ds
            return (PObject p, v)
        , do
            v <- identifier
            return (PSelf, v)
        ]

    dump ("single", t, v)

    return $ PSingle (hash v) v t
  where
    cLast (Dispatch _ (ESingle _ n _)) = return n
    cLast _ = fail "last target of dispatch chain is not a single messsage"

    cInit (Dispatch _ (ESingle _ _ x)) = return x
    cInit _ = fail "last target of dispatch chain is not a single messsage"

    -- patterns that would otherwise be mistaken for expressions
    -- if the pdCascade pattern were to grab them
    pNonExpr = choice
        [ try ppNamedSensitive
        , try ppHeadTail
        , try ppMatch
        , ppList
        , ppString
        , ppParticle
        , ppWildcard
        , parens pNonExpr
        ]


ppKeywords :: Parser Pattern
ppKeywords = keywords PKeyword PSelf pObjectPattern

ppNamed :: Parser Pattern
ppNamed = parens $ do
    name <- identifier
    lexeme colon
    p <- pPattern
    return $ PNamed name p

ppNamedSensitive :: Parser Pattern
ppNamedSensitive = parens $ do
    name <- lowIdentifier
    dump ("got ppNamedSensitive", name)
    lexeme colon
    p <- pObjectPattern
    dump ("finished ppNamedSensitive", name, p)
    return $ PNamed name p

ppObject :: Parser Pattern
ppObject = choice
    [ try $ parens ppHeadTail
    , do
        p <- name <|> parens pExpr
        return $ PObject p
    ]
  where
    name = do
        pos <- getPosition
        n <- capIdentifier
        return $ Dispatch (Just pos) (ESingle (hash n) n (ETop (Just pos)))

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
ppWildcard = symbol "_" >> return PAny

ppMatch :: Parser Pattern
ppMatch = do
    v <- pPrim
    return $ PMatch v

ppHeadTail :: Parser Pattern
ppHeadTail = do
    dump ("trying pHeadTail")
    h <- pHead
    dump ("got head", h)
    dot
    t <- pPattern
    dump ("got tail", t)
    return $ PHeadTail h t
  where
    pHead = choice
        [ try ppNamed
        , try ppMatch
        , ppList
        , ppString
        , ppParticle
        , ppAny
        , parens pHead
        ]

ppList :: Parser Pattern
ppList = brackets $ do
    ps <- commaSep pPattern
    return $ PList ps

ppString :: Parser Pattern
ppString = do
    cs <- stringLiteral
    return $ PList (map (PMatch . Char) cs)

ppParticle :: Parser Pattern
ppParticle = do
    char '@'
    try keywordParticle <|> singleParticle
  where
    singleParticle = fmap PPMSingle anyIdentifier

    keywordParticle = parens $ do
        ks <- many1 (keyword pPattern)
        let (ns, ps) = unzip ks
        return $ PPMKeyword ns (PAny:ps)
