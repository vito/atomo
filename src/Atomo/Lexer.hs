{-# LANGUAGE TypeSynonymInstances #-}
module Atomo.Lexer where

import Control.Monad.State
import Data.Char (isAlpha)
import Text.Parsec

import Atomo.Lexer.Base
import Atomo.Lexer.Primitive


lToken :: Lexer Token
lToken = choice
    [ lKeyword
    , lReserved
    , lOptional
    , lOperator
    , lParticle
    , lPrimitive
    , lMagicQuote
    , lPunctuation
    , lEnd
    ]

lReserved :: Lexer Token
lReserved = try $ do
    n <- anyIdent
    if isReservedName n
        then return (TokReserved n)
        else fail "not reserved"

lKeyword :: Lexer Token
lKeyword = try $ do
    n <- ident
    char ':'
    return (TokKeyword n)

lOptional :: Lexer Token
lOptional = try $ do
    char '&'
    n <- ident
    char ':'
    return (TokOptional n)

lParticle :: Lexer Token
lParticle = try $ do
    char '@'
    ks <- choice
        [ many1 . try $ do
            n <- ident
            char ':'
            return n
        , fmap (:[]) operator
        ]
    return (TokParticle ks)

lOperator :: Lexer Token
lOperator = try $ do
    o <- operator
    whiteSpace1 <|> eof <|> (lookAhead (lEnd <|> lPunctuation) >> return ())
    return (TokOperator o)

lPrimitive :: Lexer Token
lPrimitive = liftM TokPrimitive $ choice
    [ lvChar
    , lvString
    , try lvRational
    , try lvDouble
    , try lvInteger
    , try lvBoolean
    ]

lPunctuation :: Lexer Token
lPunctuation = choice
    [ liftM TokPunctuation (oneOf "|`'~@")
    , liftM TokOpen (oneOf "([{")
    , liftM TokClose (oneOf ")]}")
    ]

lEnd :: Lexer Token
lEnd = do
    oneOf ",;"
    return TokEnd

-- | Parse an identifier, possibly followed by a magic quote segment.
lMagicQuote :: Lexer Token
lMagicQuote = do
    name <- ident
    choice
        [ do
            str <- delimited "({[\"$|`'~@"
            flags <- many (satisfy isAlpha)
            return (TokMagicQuote name str flags)
        , return (TokIdentifier name)
        ]
  where
    delimited ds = do
        o <- oneOf ds
        cs <- many $ choice
            [ try $ do
                char '\\'
                oneOf [o, close o]
            , noneOf [o, close o]
            ]
        char (close o)
        return cs

    close '(' = ')'
    close '{' = '}'
    close '[' = ']'
    close '<' = '>'
    close x = x

lexer :: Lexer [TaggedToken]
lexer = whiteSpace >> getPosition >>= subLexer

subLexer :: SourcePos -> Lexer [TaggedToken]
subLexer start = do
    ios <- fmap lsInsideOf getState

    liftM concat $ many $ do
        i <- getPosition
        os <- fmap lsInsideOf getState
        if length os < length ios || sourceColumn i < sourceColumn start
            then fail "finished sublexer"
            else segment i

-- A sequence of tokens with a given start indentation level.
segment :: SourcePos -> Lexer [TaggedToken]
segment i = do
    t <- tagged lToken
    whiteSpace
    choice
        [ eof >> return (if tToken t == TokEnd then [t] else [t, withTag t TokEnd])
        , do
            p <- getPosition
            case tToken t of
                TokOpen o -> do
                    modifyState $ \ls -> ls { lsInsideOf = (i, o) : lsInsideOf ls }
                    n <- subLexer i
                    p' <- getPosition
                    if chainContinue i p'
                        then choice
                            [ eof >> return (t : n)
                            , do
                                ts <- segment i
                                return (t : n ++ ts)
                            ]
                        else return (t:n)
                TokClose c -> do
                    os <- fmap lsInsideOf getState
                    if not (null os) && matchWrap (snd $ head os) c
                        then modifyState $ \ls -> ls { lsInsideOf = tail os }
                        else fail $ "unmatched " ++ [c]

                    if chainContinue (fst (head os)) p || (not (chainContinue (fst (head os)) i) && chainContinue i p)
                        then return [t]
                        else return [t, withTag t TokEnd]
                _ | chainContinue i p -> do
                    ts <- segment i
                    return (t:ts)
                TokEnd -> return [t]
                _ -> return [t, withTag t TokEnd]
        ]
  where
    chainContinue o n = or
        [ sourceLine o == sourceLine n
        , sourceColumn o < sourceColumn n
        ]

matchWrap :: Char -> Char -> Bool
matchWrap '(' ')' = True
matchWrap '{' '}' = True
matchWrap '[' ']' = True
matchWrap _ _ = False

fileLexer :: Lexer [TaggedToken]
fileLexer = do
    optional (string "#!" >> manyTill anyToken (eol <|> eof))
    lexer
