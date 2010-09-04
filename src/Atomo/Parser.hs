module Atomo.Parser where

import Data.Hashable (hash)
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String

import Atomo.Debug
import Atomo.Parser.Base
import {-# SOURCE #-} Atomo.Parser.Pattern
import Atomo.Parser.Primitive
import Atomo.Types

-- the types of values in Dispatch syntax
data Dispatch
    = DParticle EParticle
    | DNormal Expr
    deriving Show

pExpr :: Parser Expr
pExpr = try pDefine <|> try pSet <|> try pDispatch <|> pLiteral <|> parens pExpr
    <?> "expression"

pLiteral :: Parser Expr
pLiteral = try pBlock <|> try pString <|> try pList <|> try pParticle <|> pPrimitive
    <?> "block, list, or primitive"

pParticle :: Parser Expr
pParticle = tagged $ do
    char '@'
    c <- choice
        [ try (cSingle True)
        , try (cKeyword True)
        , try binary
        , try symbols
        ]
    return (EParticle Nothing c)
  where
    binary = do
        op <- operator
        return $ EPMKeyword [op] [Nothing, Nothing]

    symbols = do
        names <- many1 (anyIdentifier >>= \n -> char ':' >> return n)
        spacing
        return $ EPMKeyword names (replicate (length names + 1) Nothing)

cSingle :: Bool -> Parser EParticle
cSingle p = do
    n <- if p then anyIdent else ident
    dump ("got single identifier", n)
    notFollowedBy colon
    spacing
    return (EPMSingle n)

singleDispatch :: Parser Expr
singleDispatch = tagged $ do
    pos <- getPosition
    n <- identifier
    return (Dispatch Nothing (ESingle (hash n) n (ETop (Just pos))))

cKeyword :: Bool -> Parser EParticle
cKeyword wc = do
    ks <- parens . many1 $ keyword keywordVal
    let (ns, vs) = unzip ks
    return $ EPMKeyword ns (Nothing:vs)
  where
    keywordVal
        | wc = value <|> wildcard
        | otherwise = value

    value = fmap Just $ choice
        [ pLiteral
        , pExpr
        , singleDispatch
        , parens pExpr
        ]

    wildcard = symbol "_" >> return Nothing

pDefine :: Parser Expr
pDefine = tagged (do
    dump ("trying define")
    pattern <- ppDefine
    dump ("define pattern", pattern)
    reservedOp ":="
    whiteSpace
    expr <- pExpr
    return $ Define Nothing pattern expr)
    <?> "definition"

pSet :: Parser Expr
pSet = tagged (do
    pattern <- ppSet
    dump ("set pattern", pattern)
    reservedOp "="
    whiteSpace
    expr <- pExpr
    return $ Set Nothing pattern expr)
    <?> "set"

pDispatch :: Parser Expr
pDispatch = choice
    [ try pdKeys
    , pdCascade
    ]
    <?> "dispatch"

-- TODO: find "match:" here
pdKeys :: Parser Expr
pdKeys = do
    dump "trying keys"
    pos <- getPosition
    ks <- keywords EKeyword (ETop (Just pos)) pdCascade
    dump ("got keywords", ks)
    dump ("to binary operators", toBinaryOps ks)
    return $ Dispatch (Just pos) (toBinaryOps ks)
  where
    branch = do
        pat <- pPattern
        delimit "->"
        whiteSpace
        val <- pExpr
        return (pat, val)

pdCascade :: Parser Expr
pdCascade = do
    dump "trying cascade"
    pos <- getPosition
    start <- fmap DNormal (try pLiteral <|> pCall <|> parens pExpr) <|> cascaded
    chain <- wsMany cascaded
    dump ("got cascade", start:chain)
    return $ dispatches pos (start:chain)
  where
    cascaded = fmap DParticle $ choice
        [ try (cSingle False)
        , try (cKeyword False)
        ]

    -- start off by dispatching on either a primitive or Top
    dispatches :: SourcePos -> [Dispatch] -> Expr
    dispatches _ [] = error "impossible: dispatches got empty list"
    dispatches p (DNormal e:ps) =
        dispatches' p ps e
    dispatches p (DParticle (EPMSingle n):ps) =
        dispatches' p ps (Dispatch (Just p) $ ESingle (hash n) n (ETop (Just p)))
    dispatches p (DParticle (EPMKeyword ns (Nothing:es)):ps) =
        dispatches' p ps (Dispatch (Just p) $ EKeyword (hash ns) ns (ETop (Just p):map fromJust es))

    -- roll a list of partial messages into a bunch of dispatches
    dispatches' :: SourcePos -> [Dispatch] -> Expr -> Expr
    dispatches' _ [] acc = acc
    dispatches' p (DParticle (EPMKeyword ns (Nothing:es)):ps) acc =
        dispatches' p ps (Dispatch (Just p) $ EKeyword (hash ns) ns (acc : map fromJust es))
    dispatches' p (DParticle (EPMSingle n):ps) acc =
        dispatches' p ps (Dispatch (Just p) $ ESingle (hash n) n acc)
    dispatches' _ x y = error $ "no dispatches' for: " ++ show (x, y)

pList :: Parser Expr
pList = tagged . fmap (EList Nothing) $ brackets (commaSep pExpr)

pString :: Parser Expr
pString = tagged $ do
    pos <- getPosition
    fmap (EList Nothing . map (\c -> Primitive (Just pos) (Char c))) $ stringLiteral

pBlock :: Parser Expr
pBlock = dump "trying pBlock" >> tagged . braces $ do
    arguments <- option [] . try $ do
        ps <- many1 pPattern
        delimit "|"
        whiteSpace
        return ps

    code <- wsBlock pExpr

    return $ EBlock Nothing arguments code

pCall :: Parser Expr
pCall = tagged $ reserved "dispatch" >> return (EDispatchObject Nothing)

-- 1 * 2 + 3 => (1 * 2) + 3
-- 1 + 2 * 3 => (1 + 2) * 3
-- 1 a: 2 + 3 b: 4 => (1 a: 2) + (3 b: 4)
-- 1 x: 2 || 3 y: 4 || 5 z: 6 => (1 x: 2) || ((3 y: 4) || (5 z: 6))
toBinaryOps :: EMessage -> EMessage
toBinaryOps done@(EKeyword _ [_] [_, _]) = done
toBinaryOps (EKeyword h (n:ns) (v:vs))
    | isOperator n =
        toBinaryOps . EKeyword (hash ns) ns $
            (Dispatch (eLocation v) (EKeyword (hash [n]) [n] [v, head vs]):tail vs)
    | nonOperators == ns = EKeyword h (n:ns) (v:vs)
    | null nonOperators && length vs > 2 =
        EKeyword (hash [head ns]) [head ns]
            [ Dispatch (eLocation v) $ EKeyword (hash [n]) [n] [v, head vs]
            , Dispatch (eLocation v) $ toBinaryOps (EKeyword (hash (tail ns)) (tail ns) (tail vs))
            ]
    | otherwise =
        EKeyword (hash (n:nonOperators))
            (n : nonOperators)
            (concat
                [ [v]
                , take numNonOps vs
                , [ Dispatch (eLocation v) $ toBinaryOps
                        (EKeyword (hash (drop numNonOps ns))
                            (drop numNonOps ns)
                            (drop numNonOps vs)) ]
                ])
  where
    numNonOps = length nonOperators
    nonOperators = takeWhile (not . isOperator) ns
    isOperator = all (`elem` opLetters)
toBinaryOps u = error $ "cannot toBinaryOps: " ++ show u

parser :: Parser [Expr]
parser = whiteSpace >> wsBlock pExpr

parseFile :: String -> IO (Either ParseError [Expr])
parseFile = parseFromFile parser

parseInput :: String -> Either ParseError [Expr]
parseInput = runParser (do { r <- parser; whiteSpace; eof; return r }) () "<input>"

parse :: Parser a -> String -> Either ParseError a
parse p = runParser p () "<parse>"
