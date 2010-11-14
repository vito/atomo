{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.QuasiQuotes
    ( p
    , e
    , es
    ) where

import "monads-fd" Control.Monad.State
import Data.Typeable
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.IO.Unsafe
import Text.Parsec
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Atomo.Core
import Atomo.Parser
import Atomo.Parser.Pattern
import Atomo.Parser.Base
import Atomo.Types

qqEnv :: Env
qqEnv = snd $ unsafePerformIO $
    runVM (initCore >> return (particle "ok")) startEnv

p :: QuasiQuoter
p = QuasiQuoter quotePatternExp undefined

e :: QuasiQuoter
e = QuasiQuoter quoteExprExp undefined

es :: QuasiQuoter
es = QuasiQuoter quoteExprsExp undefined

withLocation :: (String -> (String, Int, Int) -> a) -> (a -> Exp) -> String -> TH.ExpQ
withLocation p c s = do
    l <- TH.location
    return . c $ p s
        ( TH.loc_filename l
        , fst $ TH.loc_start l
        , snd $ TH.loc_start l
        )

parsing :: Typeable a => Parser a -> String -> (String, Int, Int) -> a
parsing p s (file, line, col) =
    -- here be dragons
    fromHaskell' "a" $ unsafePerformIO (runWith go (qqEnv))
  where
    go = liftM haskell $ continue pp "<qq>" s

    pp = do
        pos <- getPosition
        setPosition $
            flip setSourceName file $
            flip setSourceLine line $
            setSourceColumn pos col
        whiteSpace
        e <- p
        whiteSpace
        eof
        return e

quotePatternExp :: String -> TH.ExpQ
quotePatternExp = withLocation (parsing ppDefine) patternToExp

quoteExprExp :: String -> TH.ExpQ
quoteExprExp = withLocation (parsing pExpr) exprToExp

quoteExprsExp :: String -> TH.ExpQ
quoteExprsExp = withLocation (parsing (wsBlock pExpr)) (ListE . map exprToExp)

exprToExp :: Expr -> Exp
exprToExp (Define l p e) = AppE (AppE (expr "Define" l) (patternToExp p)) (exprToExp e)
exprToExp (Set l p e) = AppE (AppE (expr "Set" l) (patternToExp p)) (exprToExp e)
exprToExp (Dispatch l m) = AppE (expr "Dispatch" l) (emessageToExp m)
exprToExp (Operator l ns a p) = AppE (AppE (AppE (expr "Operator" l) (ListE (map (LitE . StringL) ns))) (assocToExp a)) (LitE (IntegerL p))
exprToExp (Primitive l v) = AppE (expr "Primitive" l) (valueToExp v)
exprToExp (EBlock l as es) =
    AppE (AppE (expr "EBlock" l) (ListE (map patternToExp as))) (ListE (map exprToExp es))
exprToExp (EVM {}) = error "cannot exprToExp EVM"
exprToExp (EList l es) =
    AppE (expr "EList" l) (ListE (map exprToExp es))
exprToExp (ETop l) =
    expr "ETop" l
exprToExp (EParticle l p) =
    AppE (expr "EParticle" l) (eparticleToExp p)
exprToExp (EMacro l p e) =
    AppE (AppE (expr "EMacro" l) (patternToExp p)) (exprToExp e)
exprToExp (EQuote l e) =
    AppE (expr "EQuote" l) (exprToExp e)
exprToExp (EUnquote l e) =
    AppE (expr "EUnquote" l) (exprToExp e)

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
valueToExp (Boolean b) = AppE (ConE (mkName "Boolean")) (ConE (mkName (show b)))
valueToExp (Char c) = AppE (ConE (mkName "Char")) (LitE (CharL c))
valueToExp (Double d) = AppE (ConE (mkName "Double")) (LitE (RationalL (toRational d)))
valueToExp (Expression e) = AppE (ConE (mkName "Expression")) (exprToExp e)
valueToExp (Integer i) = AppE (ConE (mkName "Integer")) (LitE (IntegerL i))
valueToExp (Message m) = AppE (ConE (mkName "Message")) (messageToExp m)
valueToExp (Particle p) = AppE (ConE (mkName "Particle")) (particleToExp p)
valueToExp (Pattern p) = AppE (ConE (mkName "Pattern")) (patternToExp p)
valueToExp (String s) = AppE (VarE (mkName "string")) (LitE (StringL (T.unpack s)))
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
patternToExp (PPMKeyword ns ts) =
    AppE (AppE (ConE (mkName "PPMKeyword")) (ListE (map (LitE . StringL) ns))) (ListE (map patternToExp ts))
patternToExp (PSingle i n t) =
    AppE (AppE (AppE (ConE (mkName "PSingle")) (LitE (IntegerL (fromIntegral i)))) (LitE (StringL n))) (patternToExp t)
patternToExp PThis = ConE (mkName "PThis")
patternToExp PEDefine = ConE (mkName "PEDefine")
patternToExp PESet = ConE (mkName "PESet")
patternToExp PEDispatch = ConE (mkName "PEDispatch")
patternToExp PEOperator = ConE (mkName "PEOperator")
patternToExp PEPrimitive = ConE (mkName "PEPrimitive")
patternToExp PEBlock = ConE (mkName "PEBlock")
patternToExp PEList = ConE (mkName "PEList")
patternToExp PEMacro = ConE (mkName "PEMacro")
patternToExp PEParticle = ConE (mkName "PEParticle")
patternToExp PETop = ConE (mkName "PETop")
patternToExp PEQuote = ConE (mkName "PEQuote")
patternToExp PEUnquote = ConE (mkName "PEUnquote")
patternToExp (PExpr e) = AppE (ConE (mkName "PExpr")) (exprToExp e)
patternToExp (PInstance p) = AppE (ConE (mkName "PInstance")) (patternToExp p)
patternToExp (PStrict p) = AppE (ConE (mkName "PStrict")) (patternToExp p)
