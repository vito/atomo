{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TypeSynonymInstances #-}
module Atomo.Format.Types where

import Control.Monad.RWS
import Data.Typeable
import Text.PrettyPrint
import qualified Data.Text as T

import Atomo.Pretty
import Atomo.Types


-- | The core types of formats to use.
data Segment
    -- | Arbitrary text.
    = SChunk T.Text

    -- | %s
    | SString

    -- | %d
    | SDecimal

    -- | %x
    | SHex

    -- | %o
    | SOctal

    -- | %b
    | SBinary

    -- | %r
    | SRadix

    -- | %f
    | SFloat

    -- | %e
    | SExponent

    -- | %g
    | SGeneral

    -- | %c
    | SChar

    -- | %a
    | SAsString

    -- | %v
    | SAny

    -- | %p(...)
    | SPluralize [Flagged] (Maybe [Flagged])

    -- | %l(...)
    | SLowercase [Flagged]

    -- | %c(...)
    | SCapitalize [Flagged]

    -- | %u(...)
    | SUppercase [Flagged]

    -- | %_
    | SSkip

    -- | %%
    | SIndirection

    -- | %{...}
    | SIterate [Flagged]

    -- | %^
    | SBreak

    -- | %[...]+(...)? where + = one or more and ? = optional (the default)
    | SConditional [[Flagged]] (Maybe [Flagged])

    -- | %j(...)+ where + = one or more
    | SJustify [[Flagged]]
    deriving (Show, Typeable)

-- Various modifiers, for our segments.
data Flag
    -- | FNumber
    -- The Maybe is Nothing if they used #, in which case we use the number 
    -- of remaining values.
    = FNumber (Maybe Int)

    -- | FSome symbol presumeably known by the segment.
    | FSymbol Char

    -- | FUsed by %f and %d
    | FZeroPad

    -- | FUsed by %f
    | FPrecision Int
    deriving (Eq, Show, Typeable)

type Flagged = (Segment, [Flag])

type FormatterT = RWST Format T.Text ([Value], Int)

type Formatter = FormatterT VM

type Format = [Flagged]

instance Pretty Format where
    prettyFrom _ fs = hcat (map pretty fs)

instance Pretty Flagged where
    prettyFrom _ (SChunk s, _) = text (T.unpack (T.replace "\"" "\\\"" s))
    prettyFrom _ (s, fs) = char '%' <> hcat (map pretty fs) <> pretty s

instance Pretty Flag where
    prettyFrom _ (FNumber Nothing) = char '#'
    prettyFrom _ (FNumber (Just n)) = int n
    prettyFrom _ (FSymbol c) = char c
    prettyFrom _ FZeroPad = char '0'
    prettyFrom _ (FPrecision n) = char '.' <> int n

instance Pretty Segment where
    prettyFrom _ (SChunk _) = error "pretty-printing a Chunk segment"
    prettyFrom _ SString = char 's'
    prettyFrom _ SDecimal = char 'd'
    prettyFrom _ SHex = char 'x'
    prettyFrom _ SOctal = char 'o'
    prettyFrom _ SBinary = char 'b'
    prettyFrom _ SRadix = char 'r'
    prettyFrom _ SFloat = char 'f'
    prettyFrom _ SExponent = char 'e'
    prettyFrom _ SGeneral = char 'g'
    prettyFrom _ SChar = char 'c'
    prettyFrom _ SAsString = char 'a'
    prettyFrom _ SAny = char 'v'
    prettyFrom _ (SPluralize s Nothing) = char 'p' <> parens (pretty s)
    prettyFrom _ (SPluralize s (Just p)) =
        char 'p' <> parens (pretty s) <> parens (pretty p)
    prettyFrom _ (SLowercase fs) = char 'l' <> parens (pretty fs)
    prettyFrom _ (SCapitalize fs) = char 'c' <> parens (pretty fs)
    prettyFrom _ (SUppercase fs) = char 'u' <> parens (pretty fs)
    prettyFrom _ SSkip = char '_'
    prettyFrom _ SIndirection = char '%'
    prettyFrom _ (SIterate fs) = braces (pretty fs)
    prettyFrom _ SBreak = char '^'
    prettyFrom _ (SConditional bs Nothing) =
        hcat (map (brackets . pretty) bs)
    prettyFrom _ (SConditional bs (Just d)) =
        hcat (map (brackets . pretty) bs) <> parens (pretty d)
    prettyFrom _ (SJustify fs) =
        char 'j' <> hcat (map (parens . pretty) fs)
