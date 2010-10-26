module Atomo.Parser where

import Control.Arrow (first, second)
import "monads-fd" Control.Monad.Error
import "monads-fd" Control.Monad.State
import Data.Maybe (fromJust, isJust)
import Text.Parsec
import qualified "mtl" Control.Monad.Trans as MTL

import Atomo.Debug
import Atomo.Environment
import Atomo.Method
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
pExpr = choice
    [ try pOperator
    , try pMacro
    , try pDefine
    , try pSet
    , try pDispatch
    , pLiteral
    , parens pExpr
    ]
    <?> "expression"

pLiteral :: Parser Expr
pLiteral = try pBlock <|> try pList <|> try pParticle <|> try pQuoted <|> try pUnquoted <|> pPrimitive
    <?> "literal"

pQuoted :: Parser Expr
pQuoted = tagged $ do
    char '`'
    e <- pSpacedExpr
    return (EQuote Nothing e)

pUnquoted :: Parser Expr
pUnquoted = tagged $ do
    char '~'
    e <- pSpacedExpr
    return (EUnquote Nothing e)

pSpacedExpr :: Parser Expr
pSpacedExpr = try pLiteral <|> simpleDispatch <|> parens pExpr
  where
    simpleDispatch = tagged $ do
        name <- ident
        notFollowedBy (char ':')
        return (Dispatch Nothing (esingle name (ETop Nothing)))

pMacro :: Parser Expr
pMacro = tagged (do
    reserved "macro"
    (Define { ePattern = p, eExpr = e }) <- pDefine
    addMacro p e
    return (EMacro Nothing p e))
    <?> "macro definition"

addMacro :: Pattern -> Expr -> Parser ()
addMacro p e =
    case p of
        PSingle {} ->
            modifyState $ \ps -> ps
                { psMacros = (addMethod (Macro p e) (fst (psMacros ps)), snd (psMacros ps))
                }

        PKeyword {} ->
            modifyState $ \ps -> ps
                { psMacros = (fst (psMacros ps), addMethod (Macro p e) (snd (psMacros ps)))
                }

        _ -> error $ "impossible: addMacro: p is " ++ show p

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
        modifyState (\ps -> ps { psOperators = (name, info) : psOperators ps })

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
        names <- many1 (anyIdent >>= \n -> char ':' >> return n)
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
    ops <- fmap psOperators getState
    return $ Dispatch (Just pos) (toBinaryOps ops msg)
    <?> "keyword dispatch"
  where
    headless = do
        p <- getPosition
        msg <- ckeywd p
        ops <- fmap psOperators getState
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
        (fmap DNormal (try pLiteral <|> parens pExpr) <|> cascaded)
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
    let (ns, mvs) = second (Nothing:) $ unzip ks
    if any isOperator (tail ns)
        then toDispatch ns mvs
        else return $ EPMKeyword ns mvs
    <?> "keyword segment"
  where
    keywordVal
        | wc = wildcard <|> value
        | otherwise = value

    keywordDispatch
        | wc = wildcard <|> disp
        | otherwise = disp

    value = fmap Just pdCascade
    disp = fmap Just pDispatch

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
            let msg = toBinaryOps (psOperators os) $ ekeyword opers (map fromJust opVals)
            return . EPMKeyword nonOpers $
                partVals ++ [Just $ Dispatch (Just pos) msg]
        | otherwise = fail "invalid particle; toplevel operator with wildcards as values"
      where
        (nonOpers, opers) = first (n:) $ span (not . isOperator) ns
        (partVals, opVals) = splitAt (length nonOpers) mvs

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

isOperator :: String -> Bool
isOperator "" = error "isOperator: empty string"
isOperator (c:_) = c `elem` opLetters

parser :: Parser [Expr]
parser = do
    optional (string "#!" >> manyTill anyToken newline)
    whiteSpace
    es <- wsBlock pExpr
    whiteSpace
    eof
    return es

cparser :: Parser (ParserState, [Expr])
cparser = do
    r <- parser
    s <- getState
    return (s, r)

parseFile :: String -> VM [Expr]
parseFile fn = liftIO (readFile fn) >>= continue (parser >>= mapM macroExpand) fn

parseInput :: String -> VM [Expr]
parseInput s = continue (parser >>= mapM macroExpand) "<input>" s

continue :: Parser a -> String -> String -> VM a
continue p s i = do
    ps <- gets parserState
    r <- runParserT (p >>= \r -> getState >>= \ps' -> return (r, ps')) ps s i
    case r of
        Left e -> throwError (ParseError e)
        Right (ok, ps') -> do
            modify $ \e -> e { parserState = ps' }
            return ok

-- | parse input i from source s, maintaining parser state between parses
continuedParse :: String -> String -> VM [Expr]
continuedParse i s = continue parser s i

withParser :: Parser a -> VM a
withParser x = continue x "<internal>" ""

macroExpand :: Expr -> Parser Expr
macroExpand d@(Define { eExpr = e }) = do
    e' <- macroExpand e
    return d { eExpr = e' }
macroExpand s@(Set { eExpr = e }) = do
    e' <- macroExpand e
    return s { eExpr = e' }
macroExpand d@(Dispatch { eMessage = em }) = do
    ms <- liftM psMacros getState
    case em of
        ESingle i n t -> do
            nt <- macroExpand t
            case lookupMap i (fst ms) of
                Nothing -> return d { eMessage = em { emTarget = nt } }
                Just (m:_) -> do
                    Expression e <- MTL.lift $ runMethod m (Single i n (Expression nt))
                    macroExpand e
        EKeyword i ns ts -> do
            nts <- mapM macroExpand ts
            case lookupMap i (snd ms) of
                Nothing -> return d { eMessage = em { emTargets = nts } }
                Just (m:_) -> do
                    Expression e <- MTL.lift $ runMethod m (Keyword i ns (map Expression nts))
                    macroExpand e
macroExpand b@(EBlock { eContents = es }) = do
    nes <- mapM macroExpand es
    return b { eContents = nes }
macroExpand l@(EList { eContents = es }) = do
    nes <- mapM macroExpand es
    return l { eContents = nes }
macroExpand m@(EMacro { eExpr = e }) = do -- TODO: is this sane?
    e' <- macroExpand e
    return m { eExpr = e' }
macroExpand p@(EParticle { eParticle = ep }) =
    case ep of
        EPMKeyword ns mes -> do
            nmes <- forM mes $ \me ->
                case me of
                    Nothing -> return Nothing
                    Just e -> liftM Just (macroExpand e)

            return p { eParticle = EPMKeyword ns nmes }

        _ -> return p
macroExpand e = return e
