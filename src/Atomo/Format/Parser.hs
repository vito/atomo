module Atomo.Format.Parser where

import Control.Monad
import Control.Monad.Identity
import Text.Parsec
import qualified Data.Text as T

import Atomo.Format.Types
import Atomo.Lexer.Base (decimal, charEscape)


data FParserState =
    FParserState
        { fpsInsideOf :: [(SourcePos, Char)] -- delims we're inside of
        , fpsWaitingFor :: [Char] -- balanced closing delim we're waiting for
        }

type Parser = ParsecT String FParserState Identity

sChunk :: Parser Segment
sChunk = liftM (SChunk . T.pack) (many1 cont)
  where
    match '{' = '}'
    match '(' = ')'
    match '[' = ']'
    match x = error ("no matching delimiter for " ++ [x])

    cont = do
        i <- liftM fpsInsideOf getState
        w <- liftM fpsWaitingFor getState
        choice $
            [ try $ do
                char '\\'
                oneOf "%{}()[]"
            , try charEscape
            , try $ do
                char '%'
                newline
                cont
            , if not (null w) && not (null i)
                then try $ do
                    c <- char (head w)
                    if c == snd (head i) -- they opened another; close that one
                        then do
                            modifyState $ \ps -> ps { fpsInsideOf = tail i }
                            return c
                        else fail "end delim"
                else fail "not inside anything"
            , do
                p <- getPosition
                o <- oneOf "{(["
                modifyState $ \ps -> ps { fpsInsideOf = (p, match o) : i }
                return o
            , noneOf ('%':take 1 w)
            ]

sString :: Parser Segment
sString = char 's' >> return SString

sInteger :: Parser Segment
sInteger = char 'd' >> return SInteger

sHex :: Parser Segment
sHex = char 'h' >> return SHex

sOctal :: Parser Segment
sOctal = char 'o' >> return SOctal

sBinary :: Parser Segment
sBinary = char 'b' >> return SBinary

sRadix :: Parser Segment
sRadix = char 'r' >> return SRadix

sFloat :: Parser Segment
sFloat = char 'f' >> return SFloat

sExponent :: Parser Segment
sExponent = char 'e' >> return SExponent

sGeneral :: Parser Segment
sGeneral = char 'g' >> return SGeneral

sChar :: Parser Segment
sChar = char 'c' >> return SChar

sAsString :: Parser Segment
sAsString = char 'a' >> return SAsString

sAny :: Parser Segment
sAny = char 'v' >> return SAny

sPluralize :: Parser Segment
sPluralize = do
    char 'p'
    s <- nested '(' ')'
    mp <- optionMaybe (nested '(' ')')
    return (SPluralize s mp)

sLowercase :: Parser Segment
sLowercase = do
    char 'l'
    fs <- nested '(' ')'
    return (SLowercase fs)

sCharOrCapitalize :: Parser Segment
sCharOrCapitalize = do
    char 'c'
    cap <- option False (try (lookAhead (char '(' >> return True)))
    if cap
        then do
            fs <- nested '(' ')'
            return (SCapitalize fs)
        else return SChar

sUppercase :: Parser Segment
sUppercase = do
    char 'u'
    fs <- nested '(' ')'
    return (SUppercase fs)

sSkip :: Parser Segment
sSkip = char '_' >> return SSkip

sIndirection :: Parser Segment
sIndirection = char '%' >> return SIndirection

sIterate :: Parser Segment
sIterate = do
    fs <- nested '{' '}'
    return (SIterate fs)

sBreak :: Parser Segment
sBreak = char '^' >> return SBreak

sConditional :: Parser Segment
sConditional = do
    fss <- many1 (nested '[' ']')
    md <- optionMaybe (nested '(' ')')
    return (SConditional fss md)

sJustify :: Parser Segment
sJustify = do
    char 'j'
    fss <- many1 (nested '(' ')')
    return (SJustify fss)

fChunk :: Parser Flagged
fChunk = liftM (flip (,) []) sChunk

fFlagged :: Parser Flagged
fFlagged = do
    char '%'
    ms <- modifiers
    s <- segment
    return (s, ms)

segment :: Parser Segment
segment = choice
    [ sString
    , sInteger
    , sHex
    , sOctal
    , sBinary
    , sRadix
    , sFloat
    , sExponent
    , sGeneral
    , sCharOrCapitalize
    , sAsString
    , sAny
    , sPluralize
    , sLowercase
    , sUppercase
    , sSkip
    , sIndirection
    , sIterate
    , sBreak
    , sConditional
    , sJustify
    ]

modifiers :: Parser [Flag]
modifiers = do
    many $ choice
        [ char '#' >> return (FNumber Nothing)
        , try $ do
            char '0'
            lookAhead ((char '.' >> decimal) <|> decimal)
            return FZeroPad
        , try $ do
            char '.'
            i <- decimal
            return (FPrecision (fromIntegral i))
        , do
            i <- decimal
            return (FNumber (Just $ fromIntegral i))
        , liftM FSymbol (oneOf ".+*=<>,?")
        ]

parser :: Parser [Flagged]
parser = many1 (choice [fFlagged, fChunk])

-- grab text between characters, balanced
nested :: Char -> Char -> Parser [Flagged]
nested o c = do
    char o
    modifyState $ \ps -> ps { fpsWaitingFor = c : fpsWaitingFor ps }
    fs <- option [] parser
    char c
    modifyState $ \ps -> ps { fpsWaitingFor = tail (fpsWaitingFor ps) }
    return fs
