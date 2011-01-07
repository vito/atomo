module Atomo.Parser.Expr where

import Control.Monad.State
import Data.Maybe (fromJust)
import Text.Parsec

import Atomo.Parser.Base
import Atomo.Pattern
import Atomo.Types hiding (keyword, option, particle, string)
import qualified Atomo.Types as T


-- | The default precedence for an operator (5).
defaultPrec :: Integer
defaultPrec = 5

-- | Parses any Atomo expression.
pExpr :: Parser Expr
pExpr = choice
    [ pOperator
    , pMacro
    , pForMacro
    , pDispatch
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
pPrimitive = tagged $ liftM (EPrimitive Nothing) primitive

-- | The @this@ keyword, i.e. the toplevel object literal.
pThis :: Parser Expr
pThis = tagged (reserved "this" >> return (ETop Nothing))
    <?> "this"

-- | An expression literal.
--
-- Example: @'1@, @'(2 + 2)@
pQuoted :: Parser Expr
pQuoted = tagged (do
    punctuation '\''
    e <- pSpacedExpr
    return (EPrimitive Nothing (Expression e)))
    <?> "quoted expression"

-- | An expression literal that may contain "unquotes" - expressions to splice
-- in to yield a different expression.
--
-- Examples: @`a@, @`(1 + ~(2 + 2))@
pQuasiQuoted :: Parser Expr
pQuasiQuoted = tagged (do
    punctuation '`'
    modifyState $ \ps -> ps { psInQuote = True }
    e <- pSpacedExpr
    modifyState $ \ps -> ps { psInQuote = False }
    return (EQuote Nothing e))
    <?> "quasiquoted expression"

-- | An unquote expression, used inside a quasiquote.
-- 
-- Examples: @~1@, @~(2 + 2)@
pUnquoted :: Parser Expr
pUnquoted = tagged (do
    punctuation '~'
    iq <- fmap psInQuote getState
    modifyState $ \ps -> ps { psInQuote = False }
    e <- pSpacedExpr
    modifyState $ \ps -> ps { psInQuote = iq }
    return (EUnquote Nothing e))
    <?> "unquoted expression"

-- | Any expression that fits into one lexical "space" - either a simple
-- literal value, a single dispatch to the toplevel object, or an expression in
-- parentheses.
--
-- Examples: @1@, @[1, 2]@, @a@, @(2 + 2)@
pSpacedExpr :: Parser Expr
pSpacedExpr = pLiteral <|> simpleDispatch <|> parens pExpr

-- | A single message sent to the toplevel object.
simpleDispatch :: Parser Expr
simpleDispatch = tagged $ do
    name <- identifier
    return (EDispatch Nothing (single name (ETop Nothing)))

-- | The for-macro "pragma."
--
-- Example: @for-macro 1 print@
pForMacro :: Parser Expr
pForMacro = tagged (do
    reserved "for-macro"
    e <- pExpr
    return (EForMacro Nothing e))
    <?> "for-macro expression"

-- | A macro definition.
--
-- Example: @macro (n squared) `(~n * ~n)@
pMacro :: Parser Expr
pMacro = tagged (do
    reserved "macro"
    p <- parens (liftM (fromJust . toMacroPattern) pExpr)
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

    info <- choice
        [ do
            a <- choice
                [ symbol "right" >> return ARight
                , symbol "left" >> return ALeft
                ]
            prec <- option defaultPrec integer
            return (a, prec)
        , liftM ((,) ALeft) integer
        ]

    ops <- many operator

    forM_ ops $ \name ->
        modifyState $ \ps -> ps
            { psOperators =
                (name, info) : psOperators ps
            }

    return (uncurry (EOperator Nothing ops) info))
    <?> "operator declaration"

-- | A particle literal.
--
-- Examples: @\@foo@, @\@(bar: 2)@, @\@bar:@, @\@(foo: 2 bar: _)@
pParticle :: Parser Expr
pParticle = tagged (do
    chained <- choice
        [ do
            punctuation '@'
            cSingle <|> cKeyword
        , particle
        ]

    case chained of
        CSingle n os ->
            return (EParticle Nothing $ single' n Nothing (map toOpt os))
        CKeyword ns es os ->
            return (EParticle Nothing $
                keyword' ns (map toRole (wildcard:es)) (map toOpt os)))
    <?> "particle"
  where
    wildcard = EDispatch Nothing (single "_" (ETop Nothing))

    toOpt (Option i n e) = Option i n (Just e)

    toRole (EDispatch { eMessage = Single { mName = "_", mTarget = ETop {} } }) =
        Nothing
    toRole e = Just e

-- | A comma-separated list of zero or more expressions, surrounded by square
-- brackets.
--
-- Examples: @[]@, @[1, $a]@
pList :: Parser Expr
pList = tagged (liftM (EList Nothing) (brackets (blockOf pExpr)))
    <?> "list"

-- | A block of expressions, surrounded by braces and optionally having
-- arguments.
--
-- Examples: @{ }@, @{ a b | a + b }@, @{ a = 1; a + 1 }@
pBlock :: Parser Expr
pBlock = tagged . braces $ do
    as <- option [] (try $ manyTill pSpacedExpr (punctuation '|' >> optional end))
    es <- blockOf pExpr
    return (EBlock Nothing (map (fromJust . toPattern) as) es)

-- | Parse an expression possibly up to an operator dispatch.
--
-- That is, this may be:
--  - an operator dispatch
--  - a keyword dispatch
--  - a single dispatch
--  - a literal or parenthesized expression
pDispatch :: Parser Expr
pDispatch = tagged (do
    s <- do
        h <- choice
            [ try (lookAhead (keyword <|> operator)) >> return (ETop Nothing)
            , pmSingle
            ]

        case h of
            EDispatch { eMessage = m@(Single { mOptionals = [] }) } -> do
                os <- pdOptionals
                return h { eMessage = m { mOptionals = os } }
            _ -> return h

    k <- followedBy keyword
    if k
        then keywordNext s
        else do

    o <- followedBy operator
    if o
        then operatorNext s
        else return s)
    <?> "dispatch"

-- | Optional keyword arguments.
pdOptionals :: Parser [Option Expr]
pdOptionals = do
    os <- many (optionSegment prKeyword)
    return (map (uncurry T.option) os)

-- | Parse an expr up to a single dispatch.
--
-- That is, this may end up just being a literal or a parenthesized expression.
pmSingle :: Parser Expr
pmSingle = do
    target <- pSpacedExpr

    let restOf =
            case target of
                EDispatch {} -> many
                _ -> many1

    chain <- option [] (try $ restOf (cSingle <|> cKeyword))

    if null chain
        then return target
        else return (dispatches target chain)
  where
    dispatches = foldl sendTo

    sendTo t (CSingle n os) =
        EDispatch Nothing (single' n t os)
    sendTo t (CKeyword ns es os) =
        EDispatch Nothing (keyword' ns (t:es) os)

-- | Parse an expr up to a keyword dispatch.
--
-- That is, this may end up just being a single dispatch, or a literal, or
-- a parenthesized expression, but it will not parse the rest of an operator
-- dispatch.
pmKeyword :: Parser Expr
pmKeyword = do
    t <- choice
        [ try (lookAhead keyword) >> return (ETop Nothing)
        , pmSingle
        ]

    k <- followedBy keyword
    if not k
        then return t
        else do

    phKeyword t

-- | Headless operator dispatch
phOperator :: Parser Expr
phOperator = do
    n <- operator
    t <- prOperator
    os <- pdOptionals
    return (EDispatch Nothing (keyword' [n] [ETop Nothing, t] os))

-- | Headless keyword dispatch
phKeyword :: Expr -> Parser Expr
phKeyword t = do
    (ns, ts) <- liftM unzip $ many1 (keywordSegment prKeyword)
    os <- pdOptionals
    return $ EDispatch Nothing (keyword' ns (t:ts) os)
    
-- | Keyword (non-first) roles
prKeyword :: Parser Expr
prKeyword = phOperator <|> phKeyword (ETop Nothing) <|> pmSingle

-- | Operator (non-first) roles
prOperator :: Parser Expr
prOperator = phOperator <|> phKeyword (ETop Nothing) <|> pmKeyword

-- | Parse the rest of a keyword dispatch, possibly followed by an operator
-- dispatch, with a given first role.
keywordNext :: Expr -> Parser Expr
keywordNext t = do
    keywd <- phKeyword t

    o <- followedBy operator
    if o
        then operatorNext keywd
        else return keywd

-- | Parse the rest of an operator dispatch, with a given first role.
operatorNext :: Expr -> Parser Expr
operatorNext f = do
    (ns, ts, os) <- liftM unzip3 . many1 $ do
        n <- operator
        t <- prOperator
        os <- pdOptionals
        return (n, t, os)

    ops <- fmap psOperators getState
    return (EDispatch Nothing (opChain ops ns (f:ts) os))

-- | Chained single message
cSingle :: Parser Chained
cSingle = choice
    [ liftM (flip CSingle []) identifier
    , try . parens $ do
        n <- identifier
        os <- pdOptionals
        return (CSingle n os)
    ]

-- | Chained keyword message
cKeyword :: Parser Chained
cKeyword = parens $ do
    (ns, es) <- choice
        [ liftM unzip $ many1 (keywordSegment prKeyword)
        , do
            o <- operator
            e <- prOperator
            return ([o], [e])
        ]

    os <- pdOptionals

    return (CKeyword ns es os)

-- | Work out precadence, associativity, etc. for binary dispatch.
-- Takes the operator table, a list of operators, their operands, and their
-- options, and creates a message dispatch with proper associativity/precedence
-- worked out.
--
-- Operators taking optional values are treated with highest precedence
-- regardless of their settings in the operator table.
--
-- For example, @1 -> 2 &x: 3 * 5@ is @(1 -> 2 &x: 3) * 5@, rather than
-- @1 -> (2 * 5) &x: 3@
opChain :: Operators -> [String] -> [Expr] -> [[Option Expr]] -> Message Expr
opChain _ [] [EDispatch { eMessage = done }] [] = done
opChain _ [a] [w, x] [opts] = keyword' [a] [w, x] opts
opChain os (a:b:cs) (w:x:y:zs) (aopts:bopts:opts)
    | nextFirst =
        keyword' [a] [w, disp $ opChain os (b:cs) (x:y:zs) (bopts:opts)] aopts
    | otherwise =
        opChain os (b:cs) (disp (keyword' [a] [w, x] aopts):y:zs) (bopts:opts)
  where
    disp = EDispatch Nothing

    nextFirst =
        null aopts && (prec b > prec a || (assoc a == ARight && prec b == prec a))

    assoc o = maybe ALeft fst (lookup o os)
    prec o = maybe defaultPrec snd (lookup o os)
opChain _ ns ts oss = error $ "opChain: " ++ show (ns, ts, oss)

-- | Parse a block of expressions from a given input string.
parser :: Parser [Expr]
parser = do
    es <- blockOf pExpr
    eof
    return es
