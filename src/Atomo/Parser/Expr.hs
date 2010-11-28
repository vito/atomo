module Atomo.Parser.Expr where

import Control.Arrow (first, second)
import Control.Monad.State
import Data.Maybe (fromJust, isJust)
import Text.Parsec
import qualified Control.Monad.Trans as MTL

import Atomo.Environment
import Atomo.Helpers (toPattern', toMacroPattern')
import Atomo.Parser.Base
import Atomo.Parser.Expand
import Atomo.Parser.Primitive
import Atomo.Types hiding (keyword, string)
import qualified Atomo.Types as T


-- | The types of values in Dispatch syntax.
data Dispatch
    = DParticle (Particle Expr)
    | DNormal Expr
    deriving Show

-- | The default precedence for an operator (5).
defaultPrec :: Integer
defaultPrec = 5

-- | Parses any Atomo expression.
pExpr :: Parser Expr
pExpr = choice
    [ pOperator
    , pMacro
    , pForMacro
    , try pDispatch
    , pLiteral
    , parens pExpr
    ]
    <?> "expression"

-- | Parses any Atomo literal value.
pLiteral :: Parser Expr
pLiteral = choice
    [ pThis
    , pBlock
    , pList
    , pParticle
    , pQuoted
    , pQuasiQuoted
    , pUnquoted
    , pPrimitive
    ]
    <?> "literal"

-- | Parses a primitive value.
--
-- Examples: @1@, @2.0@, @3\/4@, @$d@, @\"foo\"@, @True@, @False@
pPrimitive :: Parser Expr
pPrimitive = tagged $ liftM (Primitive Nothing) pPrim

-- | The @this@ keyword, i.e. the toplevel object literal.
pThis :: Parser Expr
pThis = tagged $ reserved "this" >> return (ETop Nothing)

-- | An expression literal.
--
-- Example: @'1@, @'(2 + 2)@
pQuoted :: Parser Expr
pQuoted = tagged $ do
    char '\''
    e <- pSpacedExpr
    return (Primitive Nothing (Expression e))

-- | An expression literal that may contain "unquotes" - expressions to splice
-- in to yield a different expression.
--
-- Examples: @`a@, @`(1 + ~(2 + 2))@
pQuasiQuoted :: Parser Expr
pQuasiQuoted = tagged $ do
    char '`'
    modifyState $ \ps -> ps { psInQuote = True }
    e <- pSpacedExpr
    modifyState $ \ps -> ps { psInQuote = False }
    return (EQuote Nothing e)

-- | An unquote expression, used inside a quasiquote.
-- 
-- Examples: @~1@, @~(2 + 2)@
pUnquoted :: Parser Expr
pUnquoted = tagged $ do
    char '~'
    iq <- fmap psInQuote getState
    modifyState $ \ps -> ps { psInQuote = False }
    e <- pSpacedExpr
    modifyState $ \ps -> ps { psInQuote = iq }
    return (EUnquote Nothing e)

-- | Any expression that fits into one lexical "space" - either a simple
-- literal value, a single dispatch to the toplevel object, or an expression in
-- parentheses.
--
-- Examples: @1@, @[1, 2]@, @a@, @(2 + 2)@
pSpacedExpr :: Parser Expr
pSpacedExpr = pLiteral <|> simpleDispatch <|> parens pExpr
  where
    simpleDispatch = tagged $ do
        name <- ident
        notFollowedBy (char ':')
        spacing
        return (Dispatch Nothing (single name (ETop Nothing)))

-- | The for-macro "pragma."
--
-- Example: @for-macro 1 print@
pForMacro :: Parser Expr
pForMacro = tagged (do
    reserved "for-macro"

    whiteSpace

    e <- pExpr

    return (EForMacro Nothing e))
    <?> "for-macro expression"

-- | A macro definition.
--
-- Example: @macro (n squared) `(~n * ~n)@
pMacro :: Parser Expr
pMacro = tagged (do
    reserved "macro"

    whiteSpace

    p <- parens (pExpr >>= MTL.lift . toMacroPattern')

    whiteSpace

    e <- pExpr

    return (EMacro Nothing p e))
    <?> "macro definition"

-- | An operator "pragma" - tells the parser about precedence and associativity
-- for the given operator(s).
--
-- Examples: @operator right 0 ->@, @operator 7 * /@
pOperator :: Parser Expr
pOperator = tagged (do
    reserved "operator"

    whiteSpace

    info <- choice
        [ do
            a <- choice
                [ symbol "right" >> return ARight
                , symbol "left" >> return ALeft
                ]
            prec <- option defaultPrec (try integer)
            return (a, prec)
        , liftM ((,) ALeft) integer
        ]

    ops <- operator `sepBy1` spacing

    forM_ ops $ \name ->
        modifyState $ \ps -> ps
            { psOperators =
                (name, info) : psOperators ps
            }

    return (uncurry (Operator Nothing ops) info))
    <?> "operator pragma"

-- | A particle literal.
--
-- Examples: @\@foo@, @\@(bar: 2)@, @\@bar:@, @\@(foo: 2 bar: _)@
pParticle :: Parser Expr
pParticle = tagged (do
    char '@'
    c <- choice
        [ cKeyword True
        , binary
        , try (cSingle True)
        , symbols
        ]
    return (EParticle Nothing c))
    <?> "particle"
  where
    binary = do
        op <- operator
        return $ PMKeyword [op] [Nothing, Nothing]

    symbols = do
        names <- many1 (anyIdent >>= \n -> char ':' >> return n)
        spacing
        return $ PMKeyword names (replicate (length names + 1) Nothing)

-- | Any dispatch, both single and keyword.
pDispatch :: Parser Expr
pDispatch = try pdKeys <|> pdChain
    <?> "dispatch"

-- | A keyword dispatch.
--
-- Examples: @1 foo: 2@, @1 + 2@
pdKeys :: Parser Expr
pdKeys = do
    pos <- getPosition
    msg <- keywords T.keyword (ETop (Just pos)) (try pdChain <|> headless)
    ops <- liftM psOperators getState
    return $ Dispatch (Just pos) (toBinaryOps ops msg)
    <?> "keyword dispatch"
  where
    headless = do
        p <- getPosition
        msg <- ckeywd p
        ops <- liftM psOperators getState
        return (Dispatch (Just p) (toBinaryOps ops msg))

    ckeywd pos = do
        ks <- wsMany1 $ keyword pdChain
        let (ns, es) = unzip ks
        return $ T.keyword ns (ETop (Just pos):es)
        <?> "keyword segment"

-- | A chain of message sends, both single and chained keywords.
--
-- Example: @1 sqrt (* 2) floor@
pdChain :: Parser Expr
pdChain = do
    pos <- getPosition

    chain <- wsManyStart
        (liftM DNormal (try pLiteral <|> pThis <|> parens pExpr) <|> chained)
        chained

    return $ dispatches pos chain
    <?> "single dispatch"
  where
    chained = liftM DParticle $ choice
        [ cKeyword False
        , cSingle False
        ]

    -- start off by dispatching on either a primitive or Top
    dispatches :: SourcePos -> [Dispatch] -> Expr
    dispatches p (DNormal e:ps) =
        dispatches' p ps e
    dispatches p (DParticle (PMSingle n):ps) =
        dispatches' p ps (Dispatch (Just p) $ single n (ETop (Just p)))
    dispatches p (DParticle (PMKeyword ns (Nothing:es)):ps) =
        dispatches' p ps (Dispatch (Just p) $ T.keyword ns (ETop (Just p):map fromJust es))
    dispatches _ ds = error $ "impossible: dispatches on " ++ show ds

    -- roll a list of partial messages into a bunch of dispatches
    dispatches' :: SourcePos -> [Dispatch] -> Expr -> Expr
    dispatches' _ [] acc = acc
    dispatches' p (DParticle (PMKeyword ns (Nothing:es)):ps) acc =
        dispatches' p ps (Dispatch (Just p) $ T.keyword ns (acc : map fromJust es))
    dispatches' p (DParticle (PMSingle n):ps) acc =
        dispatches' p ps (Dispatch (Just p) $ single n acc)
    dispatches' _ x y = error $ "impossible: dispatches' on " ++ show (x, y)

-- | A comma-separated list of zero or more expressions, surrounded by square
-- brackets.
--
-- Examples: @[]@, @[1, $a]@
pList :: Parser Expr
pList = (tagged . liftM (EList Nothing) $ brackets (wsDelim "," pExpr))
    <?> "list"

-- | A block of expressions, surrounded by braces and optionally having
-- arguments.
--
-- Examples: @{ }@, @{ a b | a + b }@, @{ a = 1; a + 1 }@
pBlock :: Parser Expr
pBlock = tagged (braces $ do
    arguments <- option [] . try $ do
        ps <- many1 pSpacedExpr
        whiteSpace
        string "|"
        whiteSpace1
        mapM (MTL.lift . toPattern') ps

    code <- wsBlock pExpr

    return $ EBlock Nothing arguments code)
    <?> "block"

-- | A general "single dispatch" form, without a target.
--
-- Used for both chaines and particles.
cSingle :: Bool -> Parser (Particle Expr)
cSingle p = do
    n <- if p then anyIdent else ident
    notFollowedBy colon
    spacing
    return (PMSingle n)
    <?> "single segment"

-- | A general "keyword dispatch" form, without a head.
--
-- Used for both chaines and particles.
cKeyword :: Bool -> Parser (Particle Expr)
cKeyword wc = do
    ks <- parens $ many1 keyword'
    let (ns, mvs) = second (Nothing:) $ unzip ks
    if any isOperator (tail ns)
        then toDispatch ns mvs
        else return $ PMKeyword ns mvs
    <?> "keyword segment"
  where
    keywordVal
        | wc = wildcard <|> value
        | otherwise = value

    keywordDispatch
        | wc = wildcard <|> disp
        | otherwise = disp

    value = liftM Just pdChain
    disp = liftM Just pDispatch

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

    toDispatch [] mvs = error $ "impossible: toDispatch on [] and " ++ show mvs
    toDispatch (n:ns) mvs
        | all isJust opVals = do
            os <- getState
            pos <- getPosition
            let msg = toBinaryOps (psOperators os) $ T.keyword opers (map fromJust opVals)
            return . PMKeyword nonOpers $
                partVals ++ [Just $ Dispatch (Just pos) msg]
        | otherwise = fail "invalid particle; toplevel operator with wildcards as values"
      where
        (nonOpers, opers) = first (n:) $ break isOperator ns
        (partVals, opVals) = splitAt (length nonOpers) mvs

-- | Work out precadence, associativity, etc. for a keyword dispatch.
--
-- The input is a keyword EMessage with a mix of operators and identifiers as
-- its name, e.g. @keyword { emNames = ["+", "*", "remainder"] }@.
toBinaryOps :: Operators -> Message Expr -> Message Expr
toBinaryOps _ done@(Keyword _ [_] [_, _]) = done
toBinaryOps ops (Keyword h (n:ns) (v:vs))
    | nextFirst =
         T.keyword [n]
            [ v
            , Dispatch (eLocation v)
                (toBinaryOps ops (T.keyword ns vs))
            ]
    | isOperator n =
        toBinaryOps ops . T.keyword ns $
            (Dispatch (eLocation v) (T.keyword [n] [v, head vs]):tail vs)
    | nonOperators == ns = Keyword h (n:ns) (v:vs)
    | null nonOperators && length vs > 2 =
        T.keyword [head ns]
            [ Dispatch (eLocation v) $
                T.keyword [n] [v, head vs]
            , Dispatch (eLocation v) $
                toBinaryOps ops (T.keyword (tail ns) (tail vs))
            ]
    | otherwise =
        toBinaryOps ops . T.keyword (drop numNonOps ns) $
            (Dispatch (eLocation v) $
                T.keyword (n : nonOperators)
                (v : take (numNonOps + 1) vs)) :
                drop (numNonOps + 1) vs
  where
    numNonOps = length nonOperators
    nonOperators = takeWhile (not . isOperator) ns
    nextFirst =
        isOperator n && or
            [ null ns
            , prec next > prec n
            , assoc n == ARight && prec next == prec n
            ]
      where next = head ns

    assoc n' =
        case lookup n' ops of
            Nothing -> ALeft
            Just (a, _) -> a

    prec n' =
        case lookup n' ops of
            Nothing -> defaultPrec
            Just (_, p) -> p
toBinaryOps _ u = error $ "cannot toBinaryOps: " ++ show u

-- | Parse a block of expressions from a given input string.
parser :: Parser [Expr]
parser = do
    whiteSpace
    es <- wsBlock pExpr
    whiteSpace
    eof
    return es

-- | Same as `parser', but ignores a shebang at the start of the source.
fileParser :: Parser [Expr]
fileParser = do
    optional (string "#!" >> manyTill anyToken (eol <|> eof))
    parser

