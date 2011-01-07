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

type Parser = ParsecT [TaggedToken] ParserState Identity


gensym :: Char
gensym = '!'

keyword :: Parser String
keyword =
    tokenPrim (show . pretty) (\_ t _ -> tLocation t) $ \t ->
        case tToken t of
            TokKeyword n -> Just n
            _ -> Nothing

optionalKeyword :: Parser String
optionalKeyword =
    tokenPrim (show . pretty) (\_ t _ -> tLocation t) $ \t ->
        case tToken t of
            TokOptional n -> Just n
            _ -> Nothing

operator :: Parser String
operator =
    tokenPrim (show . pretty) (\_ t _ -> tLocation t) $ \t ->
        case tToken t of
            TokOperator n -> Just n
            _ -> Nothing

identifier :: Parser String
identifier =
    tokenPrim (show . pretty) (\_ t _ -> tLocation t) $ \t ->
        case tToken t of
            TokIdentifier n -> Just n
            _ -> Nothing

particle :: Parser Chained
particle =
    tokenPrim (show . pretty) (\_ t _ -> tLocation t) $ \t ->
        case tToken t of
            TokParticle ns ->
                Just (CKeyword ns (replicate (length ns) wildcard) [])
            _ -> Nothing
  where
    wildcard = EDispatch Nothing (T.single "_" (ETop Nothing))

primitive :: Parser Value
primitive =
    tokenPrim (show . pretty) (\_ t _ -> tLocation t) $ \t ->
        case tToken t of
            TokPrimitive v -> Just v
            _ -> Nothing

punctuation :: Char -> Parser ()
punctuation p =
    tokenPrim (show . pretty) (\_ t _ -> tLocation t) $ \t ->
        case tToken t of
            TokPunctuation c | c == p -> Just ()
            TokOpen c | c == p -> Just ()
            TokClose c | c == p -> Just ()
            _ -> Nothing

reserved :: String -> Parser ()
reserved r =
    tokenPrim (show . pretty) (\_ t _ -> tLocation t) $ \t ->
        case tToken t of
            TokReserved n | n == r -> Just ()
            _ -> Nothing

end :: Parser ()
end =
    tokenPrim (show . pretty) (\_ t _ -> tLocation t) $ \t ->
        case tToken t of
            TokEnd -> Just ()
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

symbol :: String -> Parser ()
symbol s =
    tokenPrim (show . pretty) (\_ t _ -> tLocation t) $ \t ->
        case tToken t of
            TokIdentifier n | n == s -> Just ()
            _ -> Nothing

integer :: Parser Integer
integer =
    tokenPrim (show . pretty) (\_ t _ -> tLocation t) $ \t ->
        case tToken t of
            TokPrimitive (Integer i) -> Just i
            _ -> Nothing

blockOf :: Parser a -> Parser [a]
blockOf p = sepEndBy p end

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
