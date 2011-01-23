{-# LANGUAGE FlexibleContexts #-}
module Atomo.Parser where

import Control.Monad.Identity
import Control.Monad.State
import Text.Parsec

import Atomo.Environment
import Atomo.Lexer
import Atomo.Lexer.Base
import Atomo.Parser.Base
import Atomo.Parser.Expr
import Atomo.Parser.Expand
import Atomo.Types hiding (keyword, string)


-- | Parses an input file, performs macro expansion, and returns an AST.
parseFile :: FilePath -> VM [Expr]
parseFile fn =
    liftIO (readFile fn)
        >>= continue fileLexer parser fn
        >>= nextPhase

-- | Parses an input string, performs macro expansion, and returns an AST.
parseInput :: String -> VM [Expr]
parseInput s = continue lexer parser "<input>" s >>= nextPhase

-- | Given a Parser action, a source, and the input, perform that action
-- passing the parser state between VM and Parser.
continue :: Stream x Identity t => Lexer x -> ParserOf x a -> String -> String -> VM a
continue l p s i = do
    ps <- gets parserState
    case runParser l (LexerState []) s i of
        Left e -> throwError (ParseError e)
        Right ts ->
            case runParser (p >>= \r -> getState >>= \ps' -> return (r, ps')) ps s ts of
                Left e -> throwError (ParseError e)
                Right (ok, ps') -> do
                    modify $ \e -> e { parserState = ps' }
                    return ok

-- | Parse input i from source s, maintaining parser state between parses.
continuedParse :: String -> String -> VM [Expr]
continuedParse i s = continue lexer parser s i

-- | Run an arbitrary Parser action with the VM's parser state.
withParser :: Parser a -> VM a
withParser x = continue lexer x "<internal>" ""
