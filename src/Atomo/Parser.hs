module Atomo.Parser where

import Control.Monad.State
import Text.Parsec

import Atomo.Environment
import Atomo.Parser.Base
import Atomo.Parser.Expr
import Atomo.Parser.Expand
import Atomo.Types hiding (keyword, string)


-- | Parses an input file, performs macro expansion, and returns an AST.
parseFile :: FilePath -> VM [Expr]
parseFile fn =
    liftIO (readFile fn)
        >>= continue (fileParser >>= nextPhase) fn

-- | Parses an input string, performs macro expansion, and returns an AST.
parseInput :: String -> VM [Expr]
parseInput = continue (parser >>= nextPhase) "<input>"

-- | Given a Parser action, a source, and the input, perform that action
-- passing the parser state between VM and Parser.
continue :: Parser a -> String -> String -> VM a
continue p s i = do
    ps <- gets parserState
    r <- runParserT (p >>= \r -> getState >>= \ps' -> return (r, ps')) ps s i
    case r of
        Left e -> throwError (ParseError e)
        Right (ok, ps') -> do
            modify $ \e -> e { parserState = ps' }
            return ok

-- | Parse input i from source s, maintaining parser state between parses.
continuedParse :: String -> String -> VM [Expr]
continuedParse i s = continue parser s i

-- | Run an arbitrary Parser action with the VM's parser state.
withParser :: Parser a -> VM a
withParser x = continue x "<internal>" ""
