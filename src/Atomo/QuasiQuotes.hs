{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.QuasiQuotes
    ( p
    , e
    , es
    ) where

import "monads-fd" Control.Monad.State hiding (lift)
import Data.Maybe (fromJust)
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
quotePatternExp = withLocation (parsing (liftM (fromJust . toDefinePattern) pExpr)) lift

quoteExprExp :: String -> TH.ExpQ
quoteExprExp = withLocation (parsing pExpr) lift

quoteExprsExp :: String -> TH.ExpQ
quoteExprsExp = withLocation (parsing (wsBlock pExpr)) (fmap ListE . mapM lift)

instance Lift Expr where
    lift (Define _ p e) = [| Define Nothing p e |]
    lift (Set _ p e) = [| Set Nothing p e |]
    lift (Dispatch _ m) = [| Dispatch Nothing m |]
    lift (Operator _ ns a p) = [| Operator Nothing ns a p |]
    lift (Primitive _ v) = [| Primitive Nothing v |]
    lift (EBlock _ as es) = [| EBlock Nothing as es |]
    lift (EVM {}) = error "cannot lift EVM"
    lift (EList _ es) = [| EList Nothing es |]
    lift (ETop _) = [| ETop Nothing |]
    lift (EParticle _ p) = [| EParticle Nothing p |]
    lift (EMacro _ p e) = [| EMacro Nothing p e |]
    lift (EQuote _ e) = [| EQuote Nothing e |]
    lift (EUnquote _ e) = [| EUnquote Nothing e |]

instance Lift Assoc where
    lift ALeft = [| ALeft |]
    lift ARight = [| ARight |]

instance Lift Message where
    lift (Keyword i ns vs) = [| Keyword i ns vs |]
    lift (Single i n v) = [| Single i n v |]

instance Lift Particle where
    lift (PMSingle n) = [| PMSingle n |]
    lift (PMKeyword ns vs) = [| PMKeyword ns vs |]

instance Lift EMessage where
    lift (EKeyword i ns es) = [| EKeyword i ns es |]
    lift (ESingle i n e) = [| ESingle i n e |]

instance Lift EParticle where
    lift (EPMSingle n) = [| EPMSingle n |]
    lift (EPMKeyword ns es) = [| EPMKeyword ns es |]

instance Lift Value where
    lift (Block s as es) = [| Block s as es |]
    lift (Boolean b) = [| Boolean b |]
    lift (Char c) = [| Char c |]
    lift (Double d) = [| Double $(return $ LitE (RationalL (toRational d))) |]
    lift (Expression e) = [| Expression e |]
    lift (Integer i) = [| Integer i |]
    lift (Message m) = [| Message m |]
    lift (Particle p) = [| Particle p |]
    lift (Pattern p) = [| Pattern p |]
    lift (String s) = [| String (T.pack $(return $ LitE (StringL (T.unpack s)))) |]
    lift v = error $ "no lift for: " ++ show v

instance Lift Pattern where
    lift PAny = [| PAny |]
    lift (PHeadTail h t) = [| PHeadTail h t |]
    lift (PKeyword i ns ts) = [| PKeyword i ns ts |]
    lift (PList ps) = [| PList ps |]
    lift (PMatch v) = [| PMatch v |]
    lift (PNamed n p) = [| PNamed n p |]
    lift (PObject e) = [| PObject e |]
    lift (PPMKeyword ns ts) = [| PPMKeyword ns ts |]
    lift (PSingle i n t) = [| PSingle i n t |]
    lift PThis = [| PThis |]
    lift PEDefine = [| PEDefine |]
    lift PESet = [| PESet |]
    lift PEDispatch = [| PEDispatch |]
    lift PEOperator = [| PEOperator |]
    lift PEPrimitive = [| PEPrimitive |]
    lift PEBlock = [| PEBlock |]
    lift PEList = [| PEList |]
    lift PEMacro = [| PEMacro |]
    lift PEParticle = [| PEParticle |]
    lift PETop = [| PETop |]
    lift PEQuote = [| PEQuote |]
    lift PEUnquote = [| PEUnquote |]
    lift (PExpr e) = [| PExpr e |]
    lift (PInstance p) = [| PInstance p |]
    lift (PStrict p) = [| PStrict p |]
