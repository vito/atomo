module Atomo.Parser where

import Control.Arrow (first, second)
import "monads-fd" Control.Monad.State
import Data.Maybe (fromJust, isJust)
import Text.Parsec
import qualified "mtl" Control.Monad.Trans as MTL

import Atomo.Environment
import Atomo.Method
import Atomo.Parser.Base
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
    [ pOperator
    , pMacro
    , pForMacro
    , try pDispatch
    , pLiteral
    , parens pExpr
    ]
    <?> "expression"

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

pThis :: Parser Expr
pThis = tagged $ reserved "this" >> return (ETop Nothing)

pQuoted :: Parser Expr
pQuoted = tagged $ do
    char '\''
    e <- pSpacedExpr
    return (Primitive Nothing (Expression e))

pQuasiQuoted :: Parser Expr
pQuasiQuoted = tagged $ do
    char '`'
    modifyState $ \ps -> ps { psInQuote = True }
    e <- pSpacedExpr
    modifyState $ \ps -> ps { psInQuote = False }
    return (EQuote Nothing e)

pUnquoted :: Parser Expr
pUnquoted = tagged $ do
    char '~'
    iq <- fmap psInQuote getState
    modifyState $ \ps -> ps { psInQuote = False }
    e <- pSpacedExpr
    modifyState $ \ps -> ps { psInQuote = iq }
    return (EUnquote Nothing e)

pSpacedExpr :: Parser Expr
pSpacedExpr = pLiteral <|> simpleDispatch <|> parens pExpr
  where
    simpleDispatch = tagged $ do
        name <- ident
        notFollowedBy (char ':')
        spacing
        return (Dispatch Nothing (esingle name (ETop Nothing)))

pForMacro :: Parser Expr
pForMacro = tagged (do
    reserved "for-macro"
    e <- pExpr
    macroExpand e >>= MTL.lift . eval
    return (Primitive Nothing (Expression e)))
    <?> "for-macro expression"

ppMacro :: Parser Pattern
ppMacro = pExpr >>= MTL.lift . toMacroPattern'

pMacro :: Parser Expr
pMacro = tagged (do
    reserved "macro"
    p <- parens ppMacro
    whiteSpace
    e <- pExpr
    macroExpand e >>= addMacro p
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
        [ do
            a <- choice
                [ symbol "right" >> return ARight
                , symbol "left" >> return ALeft
                ]
            prec <- option defaultPrec (try integer)
            return (a, prec)
        , liftM ((,) ALeft) integer
        ]

    ops <- commaSep1 operator

    forM_ ops $ \name ->
        modifyState (\ps -> ps { psOperators = (name, info) : psOperators ps })

    return (uncurry (Operator Nothing ops) info))
    <?> "operator pragma"

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
        return $ EPMKeyword [op] [Nothing, Nothing]

    symbols = do
        names <- many1 (anyIdent >>= \n -> char ':' >> return n)
        spacing
        return $ EPMKeyword names (replicate (length names + 1) Nothing)

pDispatch :: Parser Expr
pDispatch = do
    d <- choice
        [ try pdKeys
        , pdCascade
        ]
    notFollowedBy (reserved ":=" <|> reserved "=")
    return d
    <?> "dispatch"

pdKeys :: Parser Expr
pdKeys = do
    pos <- getPosition
    msg <- keywords ekeyword (ETop (Just pos)) (try pdCascade <|> headless)
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
        ks <- wsMany1 $ keyword pdCascade
        let (ns, es) = unzip ks
        return $ ekeyword ns (ETop (Just pos):es)
        <?> "keyword segment"

pdCascade :: Parser Expr
pdCascade = do
    pos <- getPosition

    chain <- wsManyStart
        (liftM DNormal (try pLiteral <|> pThis <|> parens pExpr) <|> cascaded)
        cascaded

    return $ dispatches pos chain
    <?> "single dispatch"
  where
    cascaded = liftM DParticle $ choice
        [ cKeyword False
        , cSingle False
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
pList = (tagged . liftM (EList Nothing) $ brackets (wsDelim "," pExpr))
    <?> "list"

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

    value = liftM Just pdCascade
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
            let msg = toBinaryOps (psOperators os) $ ekeyword opers (map fromJust opVals)
            return . EPMKeyword nonOpers $
                partVals ++ [Just $ Dispatch (Just pos) msg]
        | otherwise = fail "invalid particle; toplevel operator with wildcards as values"
      where
        (nonOpers, opers) = first (n:) $ break isOperator ns
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
        toBinaryOps ops . ekeyword (drop numNonOps ns) $
            (Dispatch (eLocation v) $
                ekeyword (n : nonOperators)
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

parser :: Parser [Expr]
parser = do
    whiteSpace
    es <- wsBlock pExpr
    whiteSpace
    eof
    return es

fileParser :: Parser [Expr]
fileParser = do
    optional (string "#!" >> manyTill anyToken (eol <|> eof))
    parser

parseFile :: String -> VM [Expr]
parseFile fn = liftIO (readFile fn) >>= continue (fileParser >>= mapM macroExpand) fn

parseInput :: String -> VM [Expr]
parseInput = continue (parser >>= mapM macroExpand) "<input>"

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
    mm <- findMacro msg
    case mm of
        Just m -> do
            modifyState $ \ps -> ps { psClock = psClock ps + 1 }

            Expression e <-
                MTL.lift (runMethod m msg >>= findExpression)

            macroExpand e

        Nothing -> do
            nem <- expanded em
            return d { eMessage = nem }
  where
    expanded (ESingle i n t) = do
        nt <- macroExpand t
        return (ESingle i n nt)
    expanded (EKeyword i ns ts) = do
        nts <- mapM macroExpand ts
        return (EKeyword i ns nts)

    msg =
        case em of
            ESingle i n t -> Single i n (Expression t)
            EKeyword i ns ts -> Keyword i ns (map Expression ts)
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
-- TODO: EUnquote?
macroExpand e = return e

-- | find a findMacro method for message `m' on object `o'
findMacro :: Message -> Parser (Maybe Method)
findMacro m = do
    ids <- MTL.lift (gets primitives)
    ms <- methods m
    maybe (return Nothing) (firstMatch ids m) (lookupMap (mID m) ms)
  where
    methods (Single {}) = liftM (fst . psMacros) getState
    methods (Keyword {}) = liftM (snd . psMacros) getState

    firstMatch _ _ [] = return Nothing
    firstMatch ids' m' (mt:mts)
        | match ids' Nothing (mPattern mt) (Message m') = return (Just mt)
        | otherwise = firstMatch ids' m' mts
