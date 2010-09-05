{-# LANGUAGE TypeSynonymInstances #-}
module Atomo.Pretty (Pretty(..), prettyStack) where

import Data.IORef
import Data.Maybe (isJust)
import Text.PrettyPrint hiding (braces)
import System.IO.Unsafe
import qualified Data.IntMap as M
import qualified Data.Vector as V

import Atomo.Types
import Atomo.Parser.Base (opLetters)


data Context
    = CNone
    | CDefine
    | CKeyword
    | CSingle
    | CArgs
    | CObject
    | CPattern
    | CList

class Pretty a where
    pretty :: a -> Doc
    prettyFrom :: Context -> a -> Doc

    pretty = prettyFrom CNone


instance Pretty Value where
    prettyFrom _ (Block _ ps es)
        | null ps = braces exprs
        | otherwise = braces $ sep (map (prettyFrom CArgs) ps) <+> char '|' <+> exprs
      where
        exprs = sep . punctuate (text ";") $ map pretty es
    prettyFrom _ (Char c) = text $ show c
    prettyFrom _ (Double d) = double d
    prettyFrom _ (Expression e) = internal "expression" $ pretty e
    prettyFrom _ (Haskell v) = internal "haskell" $ text (show v)
    prettyFrom _ (Integer i) = integer i
    prettyFrom _ (List l)
        | not (null vs) && all isChar vs = text $ show (map (\(Char c) -> c) vs)
        | otherwise = brackets . hsep . punctuate comma $ map (prettyFrom CList) vs
      where vs = V.toList (unsafePerformIO (readIORef l))
    prettyFrom _ (Message m) = internal "message" $ pretty m
    prettyFrom _ (Particle p) = char '@' <> pretty p
    prettyFrom _ (Pattern p) = internal "pattern" $ pretty p
    prettyFrom _ (Process _ tid) =
        internal "process" $ text (words (show tid) !! 1)
    prettyFrom CNone (Reference r) = pretty (unsafePerformIO (readIORef r))
    prettyFrom _ (Reference _) = internal "object" empty

instance Pretty Object where
    prettyFrom _ (Object ds (ss, ks)) = vcat
        [ internal "object" $ parens (text "delegates to" <+> pretty ds)

        , if not (M.null ss)
              then nest 2 $ vcat (flip map (M.elems ss) $ (\ms ->
                  vcat (map prettyMethod ms))) <>
                      if not (M.null ks)
                          then char '\n'
                          else empty
              else empty

        , if not (M.null ks)
              then nest 2 . vcat $ flip map (M.elems ks) $ \ms ->
                  vcat (map prettyMethod ms) <> char '\n'
              else empty
        ]
      where
        prettyMethod (Slot { mPattern = p, mValue = v }) =
            pretty p <+> text ":=" <+> prettyFrom CDefine v
        prettyMethod (Method { mPattern = p, mExpr = e }) =
            pretty p <+> text ":=" <+> prettyFrom CDefine e


instance Pretty Message where
    prettyFrom _ (Single _ n t) = prettyFrom CSingle t <+> text n
    prettyFrom _ (Keyword _ ns vs) = keywords ns vs


instance Pretty Particle where
    prettyFrom _ (PMSingle e) = text e
    prettyFrom _ (PMKeyword ns vs)
        | all (not . isJust) vs = text . concat $ map keyword ns
        | not (isJust (head vs)) =
            parens $ headlessKeywords' prettyVal ns (tail vs)
        | otherwise = parens (keywords' prettyVal ns vs)
      where
        prettyVal me =
            case me of
                Nothing -> text "_"
                Just e -> prettyFrom CKeyword e


instance Pretty Pattern where
    prettyFrom _ PAny = text "_"
    prettyFrom _ (PHeadTail h t) =
        parens $ pretty h <+> text "." <+> pretty t
    prettyFrom _ (PKeyword _ ns (PSelf:vs)) =
        headlessKeywords ns vs
    prettyFrom _ (PKeyword _ ns vs) = keywords ns vs
    prettyFrom _ (PList ps)
        | not (null ps) && all isCharMatch ps = text (show (map (\(PMatch (Char c)) -> c) ps))
        | otherwise = brackets . sep $ punctuate comma (map pretty ps)
      where
        isCharMatch (PMatch (Char _)) = True
        isCharMatch _ = False
    prettyFrom _ (PMatch v) = prettyFrom CPattern v
    prettyFrom _ (PNamed n PAny) = text n
    prettyFrom _ (PNamed n p) = parens $ text n <> colon <+> pretty p
    prettyFrom _ (PObject e) = pretty e
    prettyFrom _ (PPMSingle n) = char '@' <> text n
    prettyFrom _ (PPMKeyword ns ps)
        | all isAny ps = char '@' <> text (concat $ map keyword ns)
        | isAny (head ps) =
            char '@' <> parens (headlessKeywords' (prettyFrom CKeyword) ns (tail ps))
        | otherwise = char '@' <> parens (keywords' (prettyFrom CKeyword) ns ps)
      where
        isAny PAny = True
        isAny _ = False
    prettyFrom _ PSelf = text "<self>"
    prettyFrom _ (PSingle _ n PSelf) = text n
    prettyFrom _ (PSingle _ n p) = pretty p <+> text n


instance Pretty Expr where
    prettyFrom _ (Define _ p v) = prettyFrom CDefine p <+> text ":=" <+> prettyFrom CDefine v
    prettyFrom _ (Set _ p v)    = prettyFrom CDefine p <+> text "=" <+> prettyFrom CDefine v
    prettyFrom CKeyword (Dispatch _ m@(EKeyword {})) = parens $ pretty m
    prettyFrom c (Dispatch _ m) = prettyFrom c m
    prettyFrom c (Primitive _ v) = prettyFrom c v
    prettyFrom _ (EBlock _ ps es)
        | null ps = braces exprs
        | otherwise = braces $ sep (map pretty ps) <+> char '|' <+> exprs
      where
        exprs = sep . punctuate (text ";") $ map pretty es
    prettyFrom _ (EDispatchObject {}) = text "dispatch"
    prettyFrom _ (EVM {}) = text "<vm>"
    prettyFrom _ (EList _ es)
        | all isPrimChar es = text $ show (map (\(Primitive _ (Char c)) -> c) es)
        | otherwise = brackets . sep . punctuate comma $ map (prettyFrom CList) es
      where
        isPrimChar (Primitive _ (Char _)) = True
        isPrimChar _ = False
    prettyFrom c (EParticle _ p) = char '@' <> prettyFrom c p
    prettyFrom _ (ETop {}) = text "<top>"


instance Pretty EMessage where
    prettyFrom _ (ESingle _ n (ETop {})) = text n
    prettyFrom c (ESingle _ n t) = prettyFrom c t <+> text n
    prettyFrom _ (EKeyword _ ns (ETop {}:es)) = headlessKeywords ns es
    prettyFrom _ (EKeyword _ ns es) = keywords ns es


instance Pretty EParticle where
    prettyFrom _ (EPMSingle e) = text e
    prettyFrom _ (EPMKeyword ns es)
        | all (not . isJust) es = text . concat $ map keyword ns
        | not (isJust (head es)) =
            parens $ headlessKeywords' prettyVal ns (tail es)
        | otherwise = parens $ keywords' prettyVal ns es
      where
        prettyVal me =
            case me of
                Nothing -> text "_"
                Just e -> pretty e


instance Pretty AtomoError where
    pretty (ErrorMsg msg) = text msg
    pretty (DidNotUnderstand m) =
        text "message not understood:" $$ nest 2 (pretty m)
    pretty (ParseError e) =
        text "parse error:" $$ nest 2 (text (show e))
    pretty (Mismatch p v) =
        text "pattern" <+> char '<' <> pretty p <> char '>' <+> text "did not match value:" <+> pretty v
    {-pretty (ImportError (H.UnknownError s)) =-}
        {-text "import error:" <+> text s-}
    {-pretty (ImportError (H.WontCompile ges)) =-}
        {-text "import error:" <+> sep (map text (map H.errMsg ges))-}
    {-pretty (ImportError (H.NotAllowed s)) =-}
        {-text "import error:" <+> text s-}
    {-pretty (ImportError (H.GhcException s)) =-}
        {-text "import error:" <+> text s-}


instance Pretty Delegates where
    prettyFrom _ [] = internal "bottom" empty
    prettyFrom _ [_] = text "1 object"
    prettyFrom _ ds = text $ show (length ds) ++ " objects"



prettyStack :: Expr -> Doc
prettyStack e =
    case eLocation e of
        Nothing -> pretty e
        Just s -> text (show s) $$ nest 2 (pretty e)

internal :: String -> Doc -> Doc
internal n d = char '<' <> text n <+> d <> char '>'

braces :: Doc -> Doc
braces d = char '{' <+> d <+> char '}'

headlessKeywords' :: (a -> Doc) -> [String] -> [a] -> Doc
headlessKeywords' p (k:ks) (v:vs) =
    text (keyword k) <+> p v <+> headlessKeywords' p ks vs
headlessKeywords' _ _ _ = empty

keywords' :: (a -> Doc) -> [String] -> [a] -> Doc
keywords' p ks (v:vs) =
    p v <+> headlessKeywords' p ks vs
keywords' p _ _ = empty

headlessKeywords :: Pretty a => [String] -> [a] -> Doc
headlessKeywords = headlessKeywords' (prettyFrom CKeyword)

keywords :: Pretty a => [String] -> [a] -> Doc
keywords = keywords' (prettyFrom CKeyword)

keyword :: String -> String
keyword k
    | all (`elem` opLetters) k = k
    | otherwise                = k ++ ":"

