{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Parser.Base where

import Control.Monad.Identity
import Text.Parsec

import Atomo.Lexer.Base (TaggedToken(..), Token(..))
import Atomo.Types (Expr(..), ParserState(..), Value(..), Option(..))
import Atomo.Pretty
import qualified Atomo.Types as T


-- | A headless dispatch segment.
--
-- Used for both single dispatch chains and particles.
data Chained
    = CSingle String [Option Expr]
    | CKeyword [String] [Expr] [Option Expr]
    deriving Show

type ParserOf a = ParsecT a ParserState Identity
type Parser = ParserOf [TaggedToken]


gensym :: Char
gensym = '!'

showToken :: Token -> String
showToken TokEnd = "ending"
showToken t = show (pretty t)

withToken :: (Token -> Maybe a) -> Parser a
withToken f =
    tokenPrim
        (showToken . tToken)
        (\_ t _ -> tLocation t)
        (f . tToken)

keyword :: Parser String
keyword = withToken $ \t ->
    case t of
        TokKeyword n -> Just n
        _ -> Nothing

optionalKeyword :: Parser String
optionalKeyword = withToken $ \t ->
    case t of
        TokOptional n -> Just n
        _ -> Nothing

operator :: Parser String
operator = withToken $ \t ->
    case t of
        TokOperator n -> Just n
        _ -> Nothing

identifier :: Parser String
identifier = do
    ps <- getState
    withToken $ \t ->
        case t of
            TokIdentifier (c:n) | c == gensym && psInQuote ps ->
                Just (n ++ ":" ++ show (psClock ps))
            TokIdentifier n ->
                Just n
            _ -> Nothing

particle :: Parser Chained
particle = withToken $ \t ->
    case t of
        TokParticle ns ->
            Just (CKeyword ns (replicate (length ns) wildcard) [])
        _ -> Nothing
  where
    wildcard = EDispatch Nothing (T.single "_" (ETop Nothing))

primitive :: Parser Value
primitive = withToken $ \t ->
    case t of
        TokPrimitive v -> Just v
        _ -> Nothing

macroQuote :: Parser (String, String, [Char])
macroQuote = withToken $ \t ->
    case t of
        TokMacroQuote n r fs -> Just (n, r, fs)
        _ -> Nothing

punctuation :: Char -> Parser ()
punctuation p = withToken $ \t ->
    case t of
        TokPunctuation c | c == p -> Just ()
        TokOpen c | c == p -> Just ()
        TokClose c | c == p -> Just ()
        _ -> Nothing

reserved :: String -> Parser ()
reserved r = withToken $ \t ->
    case t of
        TokReserved n | n == r -> Just ()
        _ -> Nothing

anyReserved :: Parser String
anyReserved = withToken $ \t ->
    case t of
        TokReserved n -> Just n
        _ -> Nothing

end :: Parser ()
end = withToken $ \t ->
    case t of
        TokEnd -> Just ()
        _ -> Nothing

symbol :: String -> Parser ()
symbol s = withToken $ \t ->
    case t of
        TokIdentifier n | n == s -> Just ()
        _ -> Nothing

integer :: Parser Integer
integer = withToken $ \t ->
    case t of
        TokPrimitive (Integer i) -> Just i
        _ -> Nothing

parens :: Parser a -> Parser a
parens p = do
    punctuation '('
    r <- p
    optional end
    punctuation ')'
    return r

brackets :: Parser a -> Parser a
brackets p = do
    punctuation '['
    r <- p
    punctuation ']'
    return r

braces :: Parser a -> Parser a
braces p = do
    punctuation '{'
    r <- p
    punctuation '}'
    return r

blockOf :: Parser a -> Parser [a]
blockOf p = sepEndBy p end

blockOf1 :: Parser a -> Parser [a]
blockOf1 p = sepEndBy1 p end

keywordSegment :: Parser a -> Parser (String, a)
keywordSegment p = do
    name <- keyword
    target <- p
    return (name, target)

optionSegment :: Parser a -> Parser (String, a)
optionSegment p = do
    name <- optionalKeyword
    target <- p
    return (name, target)

tagged :: Parser Expr -> Parser Expr
tagged p = do
    pos <- getPosition
    r <- p
    return r { eLocation = Just pos }

followedBy :: Parser a -> Parser Bool
followedBy p = choice
    [ lookAhead (try p) >> return True
    , return False
    ]
