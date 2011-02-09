{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Atomo.Pretty (Pretty(..), Prettied) where

import Data.Char (isUpper)
import Data.IORef
import Data.Maybe (isNothing)
import Data.Ratio
import Data.Typeable
import System.IO.Unsafe
import Text.PrettyPrint hiding (braces)
import qualified Data.Vector as V

import Atomo.Method
import Atomo.Types hiding (keyword)
import Atomo.Lexer.Base (isOperator, Token(..), TaggedToken(..))


data Context
    = CNone
    | CDefine
    | CKeyword
    | CSingle
    | CArgs
    | CPattern
    | CList

type Prettied = Doc

deriving instance Typeable Prettied

class Pretty a where
    -- | Pretty-print a value into a Doc. Typically this should be parseable
    -- back into the original value, or just a nice user-friendly output form.
    pretty :: a -> Prettied
    prettyFrom :: Context -> a -> Prettied

    pretty = prettyFrom CNone


instance Pretty Value where
    prettyFrom _ (Block _ ps es)
        | null ps = braces exprs
        | otherwise = braces $
            sep (map (prettyFrom CArgs) ps) <+> char '|' <+> exprs
      where
        exprs = sep . punctuate (text ";") $ map pretty es
    prettyFrom _ (Boolean b) = text $ show b
    prettyFrom _ (Character c) = char '$' <> (text . tail . init $ show c)
    prettyFrom _ (Continuation _) = internal "continuation" empty
    prettyFrom _ (Double d) = double d
    prettyFrom _ (Expression e) = char '\'' <> parens (pretty e)
    prettyFrom _ (Haskell v) = internal "haskell" $ text (show v)
    prettyFrom _ (Integer i) = integer i
    prettyFrom _ (List l) =
        brackets . hsep . punctuate comma $ map (prettyFrom CList) vs
      where vs = V.toList l
    prettyFrom _ (Tuple l) =
        parens . hsep . punctuate comma $ map (prettyFrom CList) vs
      where vs = V.toList l
    prettyFrom _ (Message m) = internal "message" $ pretty m
    prettyFrom _ (Method (Slot p _)) = internal "slot" $ parens (pretty p)
    prettyFrom _ (Method (Responder p _ _)) =
        internal "responder" $ parens (pretty p)
    prettyFrom _ (Method (Macro p _)) = internal "macro" $ parens (pretty p)
    prettyFrom _ (Particle p) = char '@' <> pretty p
    prettyFrom _ (Pattern p) = internal "pattern" $ pretty p
    prettyFrom _ (Process _ tid) =
        internal "process" $ text (words (show tid) !! 1)
    prettyFrom CNone (Object { oDelegates = ds, oMethods = ms }) =
        internal "object" (parens (text "delegates to" <+> pretty ds)) $$
            nest 2 (pretty ms)
    prettyFrom _ (Rational r) =
        integer (numerator r) <> char '/' <> integer (denominator r)
    prettyFrom _ (Object {}) = internal "object" empty
    prettyFrom _ (String s) = text (show s)
    prettyFrom _ (Regexp _ s o _) = text "r{" <> text (macroEscape s) <> char '}' <> text o

instance Pretty Methods where
    prettyFrom _ ms = vcat
        [ if not (nullMap ss)
              then vcat (map (vcat . map prettyMethod) (elemsMap ss)) <>
                      if not (nullMap ks)
                          then char '\n'
                          else empty
              else empty

        , if not (nullMap ks)
              then vcat $ flip map (elemsMap ks) $ \ps ->
                  vcat (map prettyMethod ps) <> char '\n'
              else empty
        ]
      where
        (ss, ks) = unsafePerformIO (readIORef ms)

        prettyMethod (Slot { mPattern = p, mValue = v }) =
            prettyFrom CDefine p <+> text ":=" <++> prettyFrom CDefine v
        prettyMethod (Responder { mPattern = p, mExpr = e }) =
            prettyFrom CDefine p <+> text ":=" <++> prettyFrom CDefine e
        prettyMethod (Macro { mPattern = p, mExpr = e }) =
            text "macro" <+> parens (pretty p) <++> prettyFrom CDefine e

instance Pretty Pattern where
    prettyFrom _ PAny = text "_"
    prettyFrom _ (PHeadTail h t) =
        parens $ pretty h <+> text "." <+> pretty t
    prettyFrom c (PMessage m) = prettyFrom c m
    prettyFrom _ (PList ps) =
        brackets . sep $ punctuate comma (map (prettyFrom CList) ps)
    prettyFrom _ (PTuple ps) =
        parens . sep $ punctuate comma (map (prettyFrom CList) ps)
    prettyFrom _ (PMatch v) = prettyFrom CPattern v
    prettyFrom _ (PNamed n PAny) = text n
    prettyFrom _ (PNamed n p) = parens $ text n <> colon <+> pretty p
    prettyFrom _ (PObject e@(EDispatch { eMessage = msg }))
        | capitalized msg = pretty e
        | isParticular msg = pretty block
      where
        capitalized (Single { mName = n, mTarget = ETop {} }) =
            isUpper (head n)
        capitalized (Single { mTarget = EDispatch { eMessage = t@(Single {}) } }) =
            capitalized t
        capitalized _ = False

        isParticular (Keyword { mNames = ["call-in"], mTargets = [EBlock {}, ETop {}] }) =
            True
        isParticular _ = False

        block = head (mTargets msg)
    prettyFrom _ (PObject e) = parens $ pretty e
    prettyFrom _ (PInstance p) = parens $ text "->" <+> pretty p
    prettyFrom _ (PStrict p) = parens $ text "==" <+> pretty p
    prettyFrom _ (PVariable p) = parens $ text "..." <+> pretty p
    prettyFrom _ (PPMKeyword ns ps)
        | all isAny ps = char '@' <> text (concatMap keyword ns)
        | isAny (head ps) =
            char '@' <> parens (headlessKeywords ns (tail ps))
        | otherwise = char '@' <> parens (keywords ns ps)
      where
        isAny PAny = True
        isAny _ = False
    prettyFrom _ (PExpr e) = pretty (EQuote Nothing e)
    prettyFrom _ PThis = text "<this>"

    prettyFrom _ PEDispatch = text "Dispatch"
    prettyFrom _ PEOperator = text "Operator"
    prettyFrom _ PEPrimitive = text "Primitive"
    prettyFrom _ PEBlock = text "Block"
    prettyFrom _ PEList = text "List"
    prettyFrom _ PETuple = text "Tuple"
    prettyFrom _ PEMacro = text "Macro"
    prettyFrom _ PEForMacro = text "ForMacro"
    prettyFrom _ PEParticle = text "Particle"
    prettyFrom _ PETop = text "Top"
    prettyFrom _ PEQuote = text "Quote"
    prettyFrom _ PEUnquote = text "Unquote"
    prettyFrom _ PEMacroQuote = text "MacroQuote"
    prettyFrom _ PEMatch = text "Match"


instance Pretty Expr where
    prettyFrom _ (EDefine _ p v) =
        prettyFrom CDefine p <+> text ":=" <++> prettyFrom CDefine v
    prettyFrom _ (ESet _ p v)    =
        prettyFrom CDefine p <+> text "=" <++> prettyFrom CDefine v
    prettyFrom CKeyword (EDispatch _ m@(Keyword {})) = parens $ pretty m
    prettyFrom CSingle (EDispatch _ m@(Keyword {})) = parens $ pretty m
    prettyFrom c (EDispatch _ m) = prettyFrom c m
    prettyFrom _ (EOperator _ ns a i) =
        text "operator" <+> assoc a <+> integer i <+> sep (map text ns)
      where
        assoc ALeft = text "left"
        assoc ARight = text "right"
    prettyFrom c (EPrimitive _ v) = prettyFrom c v
    prettyFrom _ (EBlock _ ps es)
        | null ps = braces exprs
        | otherwise = braces $ sep (map pretty ps) <+> char '|' <+> exprs
      where
        exprs = sep . punctuate (text ";") $ map pretty es
    prettyFrom CDefine (EVM {}) = text "..."
    prettyFrom _ (EVM {}) = text "<vm>"
    prettyFrom _ (EList _ es) =
        brackets . sep . punctuate comma $ map (prettyFrom CList) es
    prettyFrom _ (ETuple _ es) =
        parens . sep . punctuate comma $ map (prettyFrom CList) es
    prettyFrom _ (EMacro _ p e) =
        text "macro" <+> parens (pretty p) <++> pretty e
    prettyFrom _ (EForMacro { eExpr = e }) = text "for-macro" <+> pretty e
    prettyFrom c (EParticle _ p) = char '@' <> prettyFrom c p
    prettyFrom _ (ETop {}) = text "this"
    prettyFrom c (EQuote _ e) = char '`' <> prettySpacedExpr c e
    prettyFrom c (EUnquote _ e) = char '~' <> prettySpacedExpr c e
    prettyFrom _ (ENewDynamic {}) =
        internal "new-dynamic" empty
    prettyFrom _ (EDefineDynamic { eName = n, eExpr = e }) =
        internal "define-dynamic" $ text n <+> pretty e
    prettyFrom _ (ESetDynamic { eName = n, eExpr = e }) =
        internal "set-dynamic" $ text n <+> pretty e
    prettyFrom _ (EGetDynamic { eName = n }) =
        internal "get-dynamic" $ text n
    prettyFrom _ (EMacroQuote _ n r f) =
        text n <> char '{' <> text (macroEscape r) <> char '}' <> text f
    prettyFrom _ (EMatch _ t bs) =
        prettyFrom CKeyword t <+> text "match:" <+> branches
      where
        branches = braces . sep . punctuate (text ";") $
            flip map bs $ \(p, e) ->
                pretty p <+> text "->" <+> pretty e

instance Pretty [Expr] where
    prettyFrom _ es = sep . punctuate (text ";") $ map pretty es

instance Pretty x => Pretty (Option x) where
    prettyFrom _ (Option _ n x) = char '&' <> text n <> char ':' <+> pretty x

instance Pretty (Message Pattern) where
    prettyFrom _ (Single { mName = n, mTarget = PThis, mOptionals = os }) =
        text n <+> sep (map pretty os)
    prettyFrom _ (Single { mName = n, mTarget = (PObject ETop {}), mOptionals = os }) =
        text n <+> sep (map pretty os)
    prettyFrom _ (Single { mName = n, mTarget = p, mOptionals = os }) =
        pretty p <+> text n <+> sep (map pretty os)
    prettyFrom _ (Keyword { mNames = ns, mTargets = (PThis:vs), mOptionals = os }) =
        headlessKeywords ns vs <+> sep (map pretty os)
    prettyFrom _ (Keyword { mNames = ns, mTargets = (PObject ETop {}:vs), mOptionals = os }) =
        headlessKeywords ns vs <+> sep (map pretty os)
    prettyFrom _ (Keyword { mNames = ns, mTargets = vs, mOptionals = os }) =
        keywords ns vs <+> sep (map pretty os)

instance Pretty (Message Value) where
    prettyFrom _ (Single { mName = n, mTarget = t, mOptionals = os }) =
        prettyFrom CSingle t <+> text n <+> sep (map pretty os)
    prettyFrom _ (Keyword { mNames = ns, mTargets = vs, mOptionals = os }) =
        keywords ns vs <+> sep (map pretty os)

instance Pretty (Message Expr) where
    prettyFrom _ (Single { mName = n, mTarget = ETop {}, mOptionals = os }) =
        text n <+> sep (map pretty os)
    prettyFrom _ (Single { mName = n, mTarget = t, mOptionals = os }) =
        prettyFrom CSingle t <+> text n <+> sep (map pretty os)
    prettyFrom _ (Keyword { mNames = ns, mTargets = (ETop {}:es), mOptionals = os }) =
        headlessKeywords ns es <+> sep (map pretty os)
    prettyFrom _ (Keyword { mNames = ns, mTargets = es, mOptionals = os }) =
        keywords ns es <+> sep (map pretty os)

instance Pretty x => Pretty (Particle x) where
    prettyFrom _ (Single { mName = n, mTarget = Nothing, mOptionals = [] }) = text n
    prettyFrom _ (Single { mName = n, mTarget = Nothing, mOptionals = os }) =
        parens (text n <+> sep (map pretty os))
    prettyFrom _ (Single { mName = n, mTarget = Just t, mOptionals = os }) =
        parens (pretty t <+> text n <+> sep (map pretty os))
    prettyFrom _ (Keyword { mNames = ns, mTargets = vs, mOptionals = os })
        | all isNothing vs && null os = text . concat $ map keyword ns
        | isNothing (head vs) =
            parens $ headlessKeywords ns (tail vs) <+> sep (map pretty os)
        | otherwise = parens $ keywords ns vs <+> sep (map pretty os)

instance Pretty x => Pretty (Maybe x) where
    prettyFrom _ Nothing = text "_"
    prettyFrom c (Just v) = prettyFrom c v

instance Pretty Delegates where
    prettyFrom _ [] = internal "bottom" empty
    prettyFrom _ [_] = text "1 object"
    prettyFrom _ ds = text $ show (length ds) ++ " objects"


instance Pretty Token where
    prettyFrom _ (TokKeyword k) = text k <> char ':'
    prettyFrom _ (TokOptional o) = char '&' <> text o <> char ':'
    prettyFrom _ (TokOptionalFlag o) = char '&' <> text o
    prettyFrom _ (TokOperator o) = text o
    prettyFrom _ (TokMacroQuote n r f) =
        text n <> char '{' <> text (macroEscape r) <> char '}' <> text f
    prettyFrom _ (TokIdentifier i) = text i
    prettyFrom _ (TokParticle ks) = char '@' <> hcat (map (text . keyword) ks)
    prettyFrom _ (TokPrimitive p) = pretty p
    prettyFrom _ (TokPunctuation c) = char c
    prettyFrom _ (TokOpen c) = char c
    prettyFrom _ (TokClose c) = char c
    prettyFrom _ (TokReserved r) = text r
    prettyFrom _ TokEnd = char ';'

instance Pretty TaggedToken where
    prettyFrom c tt = prettyFrom c (tToken tt)

type Tokens = [TaggedToken]

instance Pretty Tokens where
    prettyFrom _ ts = hsep (map pretty ts)


instance Pretty AtomoError where
    prettyFrom _ (Error v) =
        text "error:" <+> pretty v
    prettyFrom _ (ParseError e) =
        text "parse error:" <+> text (show e)
    prettyFrom _ (DidNotUnderstand m) =
        text "message not understood:" <+> pretty m
    prettyFrom _ (Mismatch a b) =
        text "mismatch:" $$ nest 2 (pretty a $$ pretty b)
    prettyFrom _ (ImportError e) =
        text "haskell interpreter:" <+> text (show e)
    prettyFrom _ (FileNotFound fn) =
        text "file not found:" <+> text fn
    prettyFrom _ (ParticleArity e g) =
        text ("particle needed " ++ show e ++ " values to complete, given " ++ show g)
    prettyFrom _ (BlockArity e g) =
        text ("block expected " ++ show e ++ " arguments, given " ++ show g)
    prettyFrom _ NoExpressions =
        text "no expressions to evaluate"
    prettyFrom _ (ValueNotFound d v) =
        text "could not find a" <+> text d <+> text "in" <+> pretty v
    prettyFrom _ (DynamicNeeded t) =
        text "expected dynamic value of type" <+> text t


internal :: String -> Doc -> Doc
internal n d = char '<' <> text n <+> d <> char '>'

braces :: Doc -> Doc
braces d = char '{' <+> d <+> char '}'

macroEscape :: String -> String
macroEscape "" = ""
macroEscape ('{':cs) = "\\{" ++ macroEscape cs
macroEscape ('}':cs) = "\\}" ++ macroEscape cs
macroEscape (c:cs) = c : macroEscape cs

headlessKeywords' :: (a -> Doc) -> [String] -> [a] -> Doc
headlessKeywords' p (k:ks) (v:vs) =
    text (keyword k) <+> p v <++> headlessKeywords'' p ks vs
headlessKeywords' _ _ _ = empty

headlessKeywords'' :: (a -> Doc) -> [String] -> [a] -> Doc
headlessKeywords'' p (k:ks) (v:vs) =
    text (keyword k) <+> p v <+++> headlessKeywords'' p ks vs
headlessKeywords'' _ _ _ = empty

keywords' :: (a -> Doc) -> [String] -> [a] -> Doc
keywords' p ks (v:vs) =
    p v <+> headlessKeywords' p ks vs
keywords' _ _ _ = empty

headlessKeywords :: Pretty a => [String] -> [a] -> Doc
headlessKeywords = headlessKeywords' (prettyFrom CKeyword)

keywords :: Pretty a => [String] -> [a] -> Doc
keywords = keywords' (prettyFrom CKeyword)

keyword :: String -> String
keyword k
    | isOperator k = k
    | otherwise    = k ++ ":"

prettySpacedExpr :: Context -> Expr -> Doc
prettySpacedExpr c e
    | needsParens e = parens (prettyFrom c e)
    | otherwise = prettyFrom c e
  where
    needsParens (EDefine {}) = True
    needsParens (ESet {}) = True
    needsParens (EDispatch { eMessage = Keyword {} }) = True
    needsParens (EDispatch { eMessage = Single { mTarget = ETop {} } }) = False
    needsParens (EDispatch { eMessage = Single {} }) = True
    needsParens _ = False


infixr 4 <++>, <+++>

-- similar to <+>, but the second half will be nested to prevent long lines
(<++>) :: Doc -> Doc -> Doc
(<++>) a b
    | length (show a ++ show b) > 80 = a $$ nest 2 b
    | otherwise = a <+> b

-- similar to <++>, but without nesting
(<+++>) :: Doc -> Doc -> Doc
(<+++>) a b
    | length (show a ++ show b) > 80 = a $$ b
    | otherwise = a <+> b
