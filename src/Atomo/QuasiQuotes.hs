{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.QuasiQuotes
    ( p
    , e
    , es
    ) where

import Control.Monad.State hiding (lift)
import Data.Maybe (fromJust)
import Data.Typeable
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.IO.Unsafe
import Text.Parsec
import qualified Language.Haskell.TH as TH

import Atomo.Core
import Atomo.Helpers (fromHaskell')
import Atomo.Lexer
import Atomo.Parser
import Atomo.Parser.Base (Parser, blockOf, end)
import Atomo.Parser.Expr
import Atomo.Pattern (toDefinePattern)
import Atomo.Types


qqEnv :: Env
qqEnv = snd $ unsafePerformIO $
    runVM (initCore >> return (particle "ok")) startEnv

-- | Pattern quasi-quoter.
p :: QuasiQuoter
p = QuasiQuoter quotePatternExp undefined undefined undefined

-- | Single expression quasi-quoter.
e :: QuasiQuoter
e = QuasiQuoter quoteExprExp undefined undefined undefined

-- | Quasi-quoter for multiple expressions (a block of code).
es :: QuasiQuoter
es = QuasiQuoter quoteExprsExp undefined undefined undefined

withLocation :: (String -> (String, Int, Int) -> a) -> (a -> Q Exp) -> String -> TH.ExpQ
withLocation p c s = do
    l <- TH.location
    c $ p s
        ( TH.loc_filename l
        , fst $ TH.loc_start l
        , snd $ TH.loc_start l
        )

parsing :: Typeable a => Parser a -> String -> (String, Int, Int) -> a
parsing p s (file, line, col) =
    -- here be dragons
    fromHaskell' $ unsafePerformIO (runWith go (qqEnv))
  where
    go = liftM haskell $ continue lexer pp "<qq>" s

    pp = do
        pos <- getPosition
        setPosition $
            flip setSourceName file $
            flip setSourceLine line $
            setSourceColumn pos col
        r <- p
        end <|> eof
        return r

quotePatternExp :: String -> TH.ExpQ
quotePatternExp = withLocation (parsing (liftM (fromJust . toDefinePattern) pExpr)) lift

quoteExprExp :: String -> TH.ExpQ
quoteExprExp = withLocation (parsing pExpr) lift

quoteExprsExp :: String -> TH.ExpQ
quoteExprsExp = withLocation (parsing (blockOf pExpr)) (fmap ListE . mapM lift)
