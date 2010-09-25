module Atomo.Parser where

import Control.Monad (forM_)
import "monads-fd" Control.Monad.Trans
import "monads-fd" Control.Monad.Cont
import "monads-fd" Control.Monad.State
import Data.Maybe (fromJust)
import Text.Parsec

import Atomo.Debug
import Atomo.Parser.Base
import {-# SOURCE #-} Atomo.Parser.Pattern
import Atomo.Parser.Primitive
import Atomo.Types hiding (keyword, string)

-- the types of values in Dispatch syntax
data Dispatch
    = DParticle EParticle
    | DNormal Expr
    deriving Show

defaultPrec :: Integer
defaultPrec = 5

pExpr :: Parser Expr
pExpr = try pOperator <|> try pDefine <|> try pSet <|> try pDispatch <|> pLiteral <|> parens pExpr
    <?> "expression"

pLiteral :: Parser Expr
pLiteral = try pBlock <|> try pList <|> try pParticle <|> pPrimitive
    <?> "literal"

pOperator :: Parser Expr
pOperator = tagged (do
    reserved "operator"

    info <- choice
        [ try $ do
            a <- choice
                [ symbol "right" >> return ARight
                , symbol "left" >> return ALeft
                ]
            prec <- option defaultPrec (try integer)
            return (a, prec)
        , fmap ((,) ALeft) integer
        ]

    ops <- commaSep1 operator

    forM_ ops $ \name ->
        modifyState ((name, info) :)

    return (Operator Nothing ops (fst info) (snd info)))
    <?> "operator pragma"

pParticle :: Parser Expr
pParticle = tagged (do
    char '@'
    c <- choice
        [ try (cSingle True)
        , try (cKeyword True)
        , try binary
        , try symbols
        ]
    return (EParticle Nothing c))
    <?> "particle"
  where
    binary = do
        op <- operator
        return $ EPMKeyword [op] [Nothing, Nothing]

    symbols = do
        names <- many1 (anyIdentifier >>= \n -> char ':' >> return n)
        spacing
        return $ EPMKeyword names (replicate (length names + 1) Nothing)

pDefine :: Parser Expr
pDefine = tagged (do
    pattern <- ppDefine
    dump ("pDefine: define pattern", pattern)
    reservedOp ":="
    whiteSpace
    expr <- pExpr
    return $ Define Nothing pattern expr)
    <?> "definition"

pSet :: Parser Expr
pSet = tagged (do
    pattern <- ppSet
    dump ("pSet: set pattern", pattern)
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

pdKeys :: Parser Expr
pdKeys = do
    pos <- getPosition
    msg <- keywords ekeyword (ETop (Just pos)) (try pdCascade <|> headless)
    ops <- getState
    return $ Dispatch (Just pos) (toBinaryOps ops msg)
    <?> "keyword dispatch"
  where
    headless = do
        p <- getPosition
        msg <- ckeywd p
        ops <- getState
        return (Dispatch (Just p) (toBinaryOps ops msg))

    ckeywd pos = do
        ks <- wsMany1 $ keyword pdCascade
        let (ns, es) = unzip ks
        return $ ekeyword ns (ETop (Just pos):es)
        <?> "keyword segment"

pdCascade :: Parser Expr
pdCascade = do
    pos <- getPosition

    chain <- wsManyStart
        (fmap DNormal (try pLiteral <|> pCall <|> parens pExpr) <|> cascaded)
        cascaded

    return $ dispatches pos chain
    <?> "single dispatch"
  where
    cascaded = fmap DParticle $ choice
        [ try (cSingle False)
        , try (cKeyword False)
        ]

    -- start off by dispatching on either a primitive or Top
    dispatches :: SourcePos -> [Dispatch] -> Expr
    dispatches p (DNormal e:ps) =
        dispatches' p ps e
    dispatches p (DParticle (EPMSingle n):ps) =
        dispatches' p ps (Dispatch (Just p) $ esingle n (ETop (Just p)))
    dispatches p (DParticle (EPMKeyword ns (Nothing:es)):ps) =
        dispatches' p ps (Dispatch (Just p) $ ekeyword ns (ETop (Just p):map fromJust es))
    dispatches _ ds = error $ "impossible: dispatches on " ++ show ds

    -- roll a list of partial messages into a bunch of dispatches
    dispatches' :: SourcePos -> [Dispatch] -> Expr -> Expr
    dispatches' _ [] acc = acc
    dispatches' p (DParticle (EPMKeyword ns (Nothing:es)):ps) acc =
        dispatches' p ps (Dispatch (Just p) $ ekeyword ns (acc : map fromJust es))
    dispatches' p (DParticle (EPMSingle n):ps) acc =
        dispatches' p ps (Dispatch (Just p) $ esingle n acc)
    dispatches' _ x y = error $ "impossible: dispatches' on " ++ show (x, y)

pList :: Parser Expr
pList = (tagged . fmap (EList Nothing) $ brackets (wsDelim "," pExpr))
    <?> "list"

pBlock :: Parser Expr
pBlock = tagged (braces $ do
    arguments <- option [] . try $ do
        ps <- many1 pPattern
        delimit "|"
        whiteSpace
        return ps

    code <- wsBlock pExpr

    return $ EBlock Nothing arguments code)
    <?> "block"

pCall :: Parser Expr
pCall = tagged (reserved "dispatch" >> return (EDispatchObject Nothing))
    <?> "dispatch object"

cSingle :: Bool -> Parser EParticle
cSingle p = do
    n <- if p then anyIdent else ident
    notFollowedBy colon
    spacing
    return (EPMSingle n)
    <?> "single segment"

cKeyword :: Bool -> Parser EParticle
cKeyword wc = do
    ks <- parens $ many1 keyword'
    let (ns, vs) = unzip ks
    return $ EPMKeyword ns (Nothing:vs)
    <?> "keyword segment"
  where
    keywordVal
        | wc = wildcard <|> value
        | otherwise = value

    keywordDispatch
        | wc = wildcard <|> dispatch
        | otherwise = dispatch

    value = fmap Just pdCascade
    dispatch = fmap Just pDispatch

    keyword' = do
        name <- try (do
            name <- ident
            char ':'
            return name) <|> operator
        whiteSpace1
        target <-
            if isOperator name
                then keywordDispatch
                else keywordVal
        return (name, target)

    wildcard = symbol "_" >> return Nothing

-- work out precadence, associativity, etc. from a stream of operators
-- the input is a keyword EMessage with a mix of operators and identifiers
-- as its name, e.g. EKeyword { emNames = ["+", "*", "remainder"] }
toBinaryOps :: Operators -> EMessage -> EMessage
toBinaryOps _ done@(EKeyword _ [_] [_, _]) = done
toBinaryOps ops (EKeyword h (n:ns) (v:vs))
    | nextFirst =
         ekeyword [n]
            [ v
            , Dispatch (eLocation v)
                (toBinaryOps ops (ekeyword ns vs))
            ]
    | isOperator n =
        toBinaryOps ops . ekeyword ns $
            (Dispatch (eLocation v) (ekeyword [n] [v, head vs]):tail vs)
    | nonOperators == ns = EKeyword h (n:ns) (v:vs)
    | null nonOperators && length vs > 2 =
        ekeyword [head ns]
            [ Dispatch (eLocation v) $
                ekeyword [n] [v, head vs]
            , Dispatch (eLocation v) $
                toBinaryOps ops (ekeyword (tail ns) (tail vs))
            ]
    | otherwise =
        ekeyword
            (n : nonOperators)
            (concat
                [ [v]
                , take numNonOps vs
                , [ Dispatch (eLocation v) $ toBinaryOps ops
                        (ekeyword
                            (drop numNonOps ns)
                            (drop numNonOps vs)) ]
                ])
  where
    numNonOps = length nonOperators
    nonOperators = takeWhile (not . isOperator) ns
    nextFirst = isOperator n && (null ns || (assoc n == ARight && prec (head ns) >= prec n) || prec (head ns) > prec n)

    assoc n' =
        case lookup n' ops of
            Nothing -> ALeft
            Just (a, _) -> a

    prec n' =
        case lookup n' ops of
            Nothing -> defaultPrec
            Just (_, p) -> p
toBinaryOps _ u = error $ "cannot toBinaryOps: " ++ show u

isOperator :: String -> Bool
isOperator = all (`elem` opLetters)

parser :: Parser [Expr]
parser = do
    optional (string "#!" >> manyTill anyToken newline)
    whiteSpace
    es <- wsBlock pExpr
    whiteSpace
    eof
    return es

cparser :: Parser (Operators, [Expr])
cparser = do
    r <- parser
    s <- getState
    return (s, r)

parseFile :: String -> IO (Either ParseError [Expr])
parseFile fn = fmap (runParser parser [] fn) (readFile fn)

parseInput :: String -> Either ParseError [Expr]
parseInput = runParser parser [] "<input>"

parse :: Parser a -> String -> Either ParseError a
parse p = runParser p [] "<parse>"

-- | parse input i from source s, maintaining parser state between parses
continuedParse :: String -> String -> VM [Expr]
continuedParse i s = do
    ps <- gets parserState
    case runParser cparser ps s i of
        Left e -> throwError (ParseError e)
        Right (ps', es) -> do
            modify $ \e -> e { parserState = ps' }
            return es
