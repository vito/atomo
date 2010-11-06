{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Char where

import Data.Char

import Atomo


load :: VM ()
load = do
    [$p|(c: Char) control?|] =: liftM Boolean (onChar isControl)
    [$p|(c: Char) space?|] =: liftM Boolean (onChar isSpace)
    [$p|(c: Char) lower?|] =: liftM Boolean (onChar isLower)
    [$p|(c: Char) upper?|] =: liftM Boolean (onChar isUpper)
    [$p|(c: Char) alpha?|] =: liftM Boolean (onChar isAlpha)
    [$p|(c: Char) alphanum?|] =: liftM Boolean (onChar isAlphaNum)
    [$p|(c: Char) print?|] =: liftM Boolean (onChar isPrint)
    [$p|(c: Char) digit?|] =: liftM Boolean (onChar isDigit)
    [$p|(c: Char) oct-digit?|] =: liftM Boolean (onChar isOctDigit)
    [$p|(c: Char) hex-digit?|] =: liftM Boolean (onChar isHexDigit)
    [$p|(c: Char) letter?|] =: liftM Boolean (onChar isLetter)
    [$p|(c: Char) mark?|] =: liftM Boolean (onChar isMark)
    [$p|(c: Char) number?|] =: liftM Boolean (onChar isNumber)
    [$p|(c: Char) punctuation?|] =: liftM Boolean (onChar isPunctuation)
    [$p|(c: Char) symbol?|] =: liftM Boolean (onChar isSymbol)
    [$p|(c: Char) separator?|] =: liftM Boolean (onChar isSeparator)
    [$p|(c: Char) ascii?|] =: liftM Boolean (onChar isAscii)
    [$p|(c: Char) latin1?|] =: liftM Boolean (onChar isLatin1)
    [$p|(c: Char) ascii-upper?|] =: liftM Boolean (onChar isAsciiLower)
    [$p|(c: Char) ascii-lower?|] =: liftM Boolean (onChar isAsciiUpper)

    [$p|(c: Char) uppercase|] =: liftM Char (onChar toUpper)
    [$p|(c: Char) lowercase|] =: liftM Char (onChar toLower)
    [$p|(c: Char) titlecase|] =: liftM Char (onChar toTitle)

    [$p|(c: Char) from-digit|] =: liftM (Integer . fromIntegral) (onChar digitToInt)
    [$p|(i: Integer) to-digit|] =: liftM Char (onInteger (intToDigit . fromIntegral))

    [$p|(c: Char) ord|] =: liftM (Integer . fromIntegral) (onChar ord)
    [$p|(i: Integer) chr|] =: liftM Char (onInteger (chr . fromIntegral))
  where
    onChar :: (Char -> a) -> VM a
    onChar f = here "c" >>= liftM (f . fromChar) . findChar

    onInteger :: (Integer -> a) -> VM a
    onInteger f = here "i" >>= liftM (f . Atomo.fromInteger) . findInteger
