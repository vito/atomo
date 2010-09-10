{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Haskell
    ( module Control.Concurrent
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Monad.Trans.Class
    , module Control.Monad.Trans.Error
    , module Control.Monad.Trans.State
    , module Atomo.Types
    , p
    , e
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error hiding (liftCallCC, liftListen, liftPass)
import Control.Monad.Trans.State hiding (liftCallCC, liftListen, liftPass)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Parsec
import qualified Language.Haskell.TH as TH

import Atomo.Parser
import Atomo.Parser.Pattern
import Atomo.Parser.Base
import Atomo.Types


p :: QuasiQuoter
p = QuasiQuoter quotePatternExp undefined

e :: QuasiQuoter
e = QuasiQuoter quoteExprExp undefined

parsePattern :: Monad m => String -> (String, Int, Int) -> m Pattern
parsePattern s (file, line, col) =
    case runParser p [] "<qq>" s of
        Left e -> fail (show e)
        Right e -> return e
  where
    p = do
        pos <- getPosition
        setPosition $
            (flip setSourceName) file $
            (flip setSourceLine) line $
            (flip setSourceColumn) col $
            pos
        whiteSpace
        p <- ppDefine
        eof
        return p

quotePatternExp :: String -> TH.ExpQ
quotePatternExp s = do
    l <- TH.location
    pat <- parsePattern s
        ( TH.loc_filename l
        , fst $ TH.loc_start l
        , snd $ TH.loc_start l
        )
    return (patternToExp pat)

parseExpr :: Monad m => String -> (String, Int, Int) -> m Expr
parseExpr s (file, line, col) =
    case runParser p [] "<qq>" s of
        Left e -> fail (show e)
        Right e -> return e
  where
    p = do
        pos <- getPosition
        setPosition $
            (flip setSourceName) file $
            (flip setSourceLine) line $
            (flip setSourceColumn) col $
            pos
        whiteSpace
        e <- pExpr
        eof
        return e

quoteExprExp :: String -> TH.ExpQ
quoteExprExp s = do
    l <- TH.location
    e <- parseExpr s
        ( TH.loc_filename l
        , fst $ TH.loc_start l
        , snd $ TH.loc_start l
        )
    return (exprToExp e)

exprToExp :: Expr -> Exp
exprToExp (Define l p e) = AppE (AppE (expr "Define" l) (patternToExp p)) (exprToExp e)
exprToExp (Set l p e) = AppE (AppE (expr "Set" l) (patternToExp p)) (exprToExp e)
exprToExp (Dispatch l m) = AppE (expr "Dispatch" l) (emessageToExp m)
exprToExp (Operator l ns a p) = AppE (AppE (AppE (expr "Operator" l) (ListE (map (LitE . StringL) ns))) (assocToExp a)) (LitE (IntegerL p))
exprToExp (Primitive l v) = AppE (expr "Primitive" l) (valueToExp v)
exprToExp (EBlock l as es) =
    AppE (AppE (expr "EBlock" l) (ListE (map patternToExp as))) (ListE (map exprToExp es))
exprToExp (EDispatchObject l) =
    expr "EDispatchObject" l
exprToExp (EVM _ _) = error "cannot exprToExp EVM"
exprToExp (EList l es) =
    AppE (expr "EList" l) (ListE (map exprToExp es))
exprToExp (ETop l) =
    expr "ETop" l
exprToExp (EParticle l p) =
    AppE (expr "EParticle" l) (eparticleToExp p)

assocToExp :: Assoc -> Exp
assocToExp ALeft = ConE (mkName "ALeft")
assocToExp ARight = ConE (mkName "ARight")

messageToExp :: Message -> Exp
messageToExp (Keyword i ns vs) =
    AppE (AppE (AppE (ConE (mkName "Keyword")) (LitE (IntegerL (fromIntegral i)))) (ListE (map (LitE . StringL) ns))) (ListE (map valueToExp vs))
messageToExp (Single i n v) =
    AppE (AppE (AppE (ConE (mkName "Single")) (LitE (IntegerL (fromIntegral i)))) (LitE (StringL n))) (valueToExp v)

particleToExp :: Particle -> Exp
particleToExp (PMSingle n) =
    AppE (ConE (mkName "PMSingle")) (LitE (StringL n))
particleToExp (PMKeyword ns vs) =
    AppE (AppE (ConE (mkName "PMKeyword")) (ListE (map (LitE . StringL) ns))) (ListE (map maybeValue vs))
  where
    maybeValue Nothing = ConE (mkName "Nothing")
    maybeValue (Just v) = AppE (ConE (mkName "Just")) (valueToExp v)

emessageToExp :: EMessage -> Exp
emessageToExp (EKeyword i ns es) =
    AppE (AppE (AppE (ConE (mkName "EKeyword")) (LitE (IntegerL (fromIntegral i)))) (ListE (map (LitE . StringL) ns))) (ListE (map exprToExp es))
emessageToExp (ESingle i n e) =
    AppE (AppE (AppE (ConE (mkName "ESingle")) (LitE (IntegerL (fromIntegral i)))) (LitE (StringL n))) (exprToExp e)

eparticleToExp :: EParticle -> Exp
eparticleToExp (EPMSingle n) =
    AppE (ConE (mkName "EPMSingle")) (LitE (StringL n))
eparticleToExp (EPMKeyword ns es) =
    AppE (AppE (ConE (mkName "EPMKeyword")) (ListE (map (LitE . StringL) ns))) (ListE (map maybeExpr es))
  where
    maybeExpr Nothing = ConE (mkName "Nothing")
    maybeExpr (Just e) = AppE (ConE (mkName "Just")) (exprToExp e)

expr :: String -> Maybe SourcePos -> Exp
expr n _ = AppE (ConE (mkName n)) (ConE (mkName "Nothing"))

valueToExp :: Value -> Exp
valueToExp (Block s as es) =
    AppE (AppE (AppE (ConE (mkName "Block")) (valueToExp s)) (ListE (map patternToExp as))) (ListE (map exprToExp es))
valueToExp (Char c) = AppE (ConE (mkName "Char")) (LitE (CharL c))
valueToExp (Double d) = AppE (ConE (mkName "Double")) (LitE (RationalL (toRational d)))
valueToExp (Expression e) = AppE (ConE (mkName "Expression")) (exprToExp e)
valueToExp (Integer i) = AppE (ConE (mkName "Integer")) (LitE (IntegerL i))
valueToExp (Message m) = AppE (ConE (mkName "Message")) (messageToExp m)
valueToExp (Particle p) = AppE (ConE (mkName "Particle")) (particleToExp p)
valueToExp (Pattern p) = AppE (ConE (mkName "Pattern")) (patternToExp p)
valueToExp v = error $ "no valueToExp for: " ++ show v

patternToExp :: Pattern -> Exp
patternToExp PAny = ConE (mkName "PAny")
patternToExp (PHeadTail h t) = AppE (AppE (ConE (mkName "PHeadTail")) (patternToExp h)) (patternToExp t)
patternToExp (PKeyword i ns ts) =
    AppE (AppE (AppE (ConE (mkName "PKeyword")) (LitE (IntegerL (fromIntegral i)))) (ListE (map (LitE . StringL) ns))) (ListE (map patternToExp ts))
patternToExp (PList ps) =
    AppE (ConE (mkName "PList")) (ListE (map patternToExp ps))
patternToExp (PMatch v) =
    AppE (ConE (mkName "PMatch")) (valueToExp v)
patternToExp (PNamed n p) =
    AppE (AppE (ConE (mkName "PNamed")) (LitE (StringL n))) (patternToExp p)
patternToExp (PObject e) =
    AppE (ConE (mkName "PObject")) (exprToExp e)
patternToExp (PPMSingle n) =
    AppE (ConE (mkName "PPMSingle")) (LitE (StringL n))
patternToExp (PPMKeyword ns ts) =
    AppE (AppE (ConE (mkName "PPMKeyword")) (ListE (map (LitE . StringL) ns))) (ListE (map patternToExp ts))
patternToExp PSelf = ConE (mkName "PSelf")
patternToExp (PSingle i n t) =
    AppE (AppE (AppE (ConE (mkName "PSingle")) (LitE (IntegerL (fromIntegral i)))) (LitE (StringL n))) (patternToExp t)
