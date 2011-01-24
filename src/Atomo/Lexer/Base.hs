module Atomo.Lexer.Base where

import Control.Monad.Identity
import Data.Char
import Data.List (nub, sort)
import Text.Parsec
import qualified Text.Parsec.Token as P

import Atomo.Types (Value)


data LexerState =
    LexerState
        { lsInsideOf :: [(SourcePos, Char)]
        }

data Token
    = TokKeyword String
    | TokOptional String
    | TokOperator String
    | TokMacroQuote String String [Char]
    | TokIdentifier String
    | TokParticle [String]
    | TokPrimitive Value
    | TokPunctuation Char
    | TokOpen Char
    | TokClose Char
    | TokReserved String
    | TokEnd
    deriving (Eq, Show)

data TaggedToken =
    TaggedToken
        { tToken :: Token
        , tLocation :: SourcePos
        }
    deriving (Eq, Show)

type Lexer = ParsecT String LexerState Identity


isOpLetter :: Char -> Bool
isOpLetter c = c `elem` "!@#%&*-./\\?:" || (c `notElem` "`" && isSymbol c)

isOperator :: String -> Bool
isOperator "" = False
isOperator cs = head cs `notElem` "@$~" && all isOpLetter cs

def :: P.GenLanguageDef String LexerState Identity
def = P.LanguageDef
    { P.commentStart = "{-"
    , P.commentEnd = "-}"
    , P.commentLine = "--"
    , P.nestedComments = True
    , P.identStart = satisfy (\c -> c == '_' || isLetter c || (c `notElem` "&@$~:" && isOpLetter c))
    , P.identLetter = satisfy (\c -> c == '_' || isAlphaNum c || (c /= ':' && isOpLetter c))
    , P.opStart = satisfy (\c -> c `notElem` "@$~" && isOpLetter c)
    , P.opLetter = satisfy isOpLetter
    , P.reservedOpNames = [",", "|"]
    , P.reservedNames = ["operator", "macro", "for-macro", "this"]
    , P.caseSensitive = True
    }

eol :: Lexer ()
eol = newline >> return ()

anyIdent :: Lexer String
anyIdent = try $ do
    c <- P.identStart def
    cs <- many (P.identLetter def)
    if isOperator (c:cs)
        then unexpected "operator"
        else do

    return (c:cs)

ident :: Lexer String
ident = do
    name <- anyIdent
    if isReservedName name
        then unexpected ("reserved word " ++ show name)
        else return name

lexeme :: Lexer a -> Lexer a
lexeme l = do
    r <- l
    whiteSpace
    return r

symbol :: String -> Lexer String
symbol = lexeme . string

identifier :: Lexer String
identifier = lexeme ident

operator :: Lexer String
operator = try $ do
    c <- P.opStart def
    cs <- many (P.opLetter def)
    if (c:cs) `elem` P.reservedOpNames def
        then unexpected ("reserved operator " ++ show (c:cs))
        else return (c:cs)

reserved :: String -> Lexer ()
reserved n = try $ do
    string n
    notFollowedBy (P.identLetter def) <?> "end of " ++ show n

integer :: Lexer Integer
integer = do
    f <- sign
    n <- natural
    return (f n)

float :: Lexer Double
float = do
    f <- sign
    n <- floating
    return (f n)

natural :: Lexer Integer
natural = zeroNumber <|> decimal

stringLiteral :: Lexer String
stringLiteral = do
    str <-
        between
            (char '"')
            (char '"' <?> "end of string")
            (many stringChar)

    return (foldr (maybe id (:)) "" str)

charLiteral :: Lexer Char
charLiteral = do
    char '$'
    charEscape <|> charLetter
    <?> "literal character"

tagged :: Lexer Token -> Lexer TaggedToken
tagged p = do
    pos <- getPosition
    r <- p
    return (TaggedToken r pos)

withTag :: TaggedToken -> Token -> TaggedToken
withTag tt t = tt { tToken = t }

isReservedName :: String -> Bool
isReservedName name = isReserved reservedNames name
  where
    reservedNames = sort (P.reservedNames def)

isReserved :: [String] -> String -> Bool
isReserved names name
    = scan names
    where
        scan [] = False
        scan (r:rs) =
            case compare r name of
                LT  -> scan rs
                EQ  -> True
                GT  -> False


-----------------------------------------------------------------------------
-- Numeric ------------------------------------------------------------------
-----------------------------------------------------------------------------

decimal :: Lexer Integer
decimal = number 10 digit

number :: Integer -> Lexer Char -> Lexer Integer
number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
    n `seq` (return n)

zeroNumber :: Lexer Integer
zeroNumber = do
    char '0'
    hexadecimal <|> octal <|> decimal <|> return 0
    <?> "zeroNumber"

hexadecimal :: Lexer Integer
hexadecimal = oneOf "xX" >> number 16 hexDigit

octal :: Lexer Integer
octal = oneOf "oO" >> number 8 octDigit

floating :: Lexer Double
floating = do
    n <- decimal
    fractExponent n

fractExponent :: Integer -> Lexer Double
fractExponent n = choice
    [ do
        fract <- fraction
        expo  <- option 1.0 exponent'
        return ((fromInteger n + fract) * expo)
    , do
        expo <- exponent'
        return (fromInteger n * expo)
    ]

fraction :: Lexer Double
fraction = do
    char '.'
    digits <- many1 digit <?> "fraction"
    return (foldr op 0.0 digits)
    <?> "fraction"
    where
    op d f  = (f + fromIntegral (digitToInt d)) / 10.0

exponent' :: Lexer Double
exponent' = do
    oneOf "eE"
    f <- sign
    e <- decimal <?> "exponent"
    return (power (f e))
    <?> "exponent"
    where
    power e  | e < 0      = 1.0 / power(-e)
            | otherwise  = fromInteger (10 ^ e)

sign :: Num a => Lexer (a -> a)
sign = choice
    [ char '-' >> return negate
    , char '+' >> return id
    , return id
    ]

-----------------------------------------------------------------------------
-- Whitespace & Comments ----------------------------------------------------
-----------------------------------------------------------------------------

whiteSpace :: Lexer ()
whiteSpace = do
    spacing
    skipMany (try $ spacing >> newline)
    spacing

whiteSpace1 :: Lexer ()
whiteSpace1 = (space <|> newline) >> whiteSpace

simpleSpace :: Lexer ()
simpleSpace = skipMany1 $ satisfy (`elem` " \t\f\v\xa0")

spacing :: Lexer ()
spacing = skipMany spacing1

spacing1 :: Lexer ()
spacing1 = choice
    [ simpleSpace
    , oneLineComment
    , multiLineComment
    ]
    <?> "whitespace or commend"

oneLineComment :: Lexer ()
oneLineComment = do
    try (string (P.commentLine def))
    skipMany (satisfy (/= '\n'))

multiLineComment :: Lexer ()
multiLineComment = do
    try (string (P.commentStart def))
    inComment

inComment :: Lexer ()
inComment = choice
    [ try (string (P.commentEnd def)) >> return ()
    , multiLineComment >> inComment
    , skipMany1 (noneOf startEnd) >> inComment
    , oneOf startEnd >> inComment
    ]
    <?> "end of comment"
  where
    startEnd = nub (P.commentEnd def ++ P.commentStart def)


-----------------------------------------------------------------------------
-- Character Escaping -------------------------------------------------------
-----------------------------------------------------------------------------

charEscape :: Lexer Char
charEscape = char '\\' >> escapeCode

charLetter :: Lexer Char
charLetter = satisfy (\c -> (c /= '\\') && (c > '\026'))

stringChar :: Lexer (Maybe Char)
stringChar = choice
    [ fmap Just stringLetter
    , stringEscape
    ]
    <?> "string character"

stringLetter :: Lexer Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape :: Lexer (Maybe Char)
stringEscape = char '\\' >> choice
    [ escapeGap >> return Nothing
    , escapeEmpty >> return Nothing
    , fmap Just escapeCode
    ]

escapeEmpty :: Lexer Char
escapeEmpty = char '&'

escapeGap :: Lexer Char
escapeGap = do
    many1 space
    char '\\' <?> "end of string gap"

escMap :: [(Char, Char)]
escMap = zip "abfnrtv\\\"" "\a\b\f\n\r\t\v\\\""

asciiMap :: [(String, Char)]
asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes :: [String]
ascii2codes = [ "BS","HT","LF","VT","FF","CR","SO","SI","EM",
                "FS","GS","RS","US","SP" ]

ascii3codes :: [String]
ascii3codes = [ "NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                "CAN","SUB","ESC","DEL" ]

ascii2 :: [Char]
ascii2 = "\b\t\n\v\f\r\SO\SI\EM\FS\GS\RS\US "

ascii3 :: [Char]
ascii3 = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"

escapeCode :: Lexer Char
escapeCode = charEsc <|> charNum <|> charAscii <|> charControl
    <?> "escape code"

charControl :: Lexer Char
charControl = do
    char '^'
    code <- upper
    return (toEnum (fromEnum code - fromEnum 'A'))

charNum :: Lexer Char
charNum = do
    code <- choice
        [ decimal
        , char 'o' >> number 8 octDigit
        , char 'x' >> number 16 hexDigit
        ]

    return (toEnum (fromInteger code))

charEsc :: Lexer Char
charEsc = choice (map parseEsc escMap)
  where
    parseEsc (c, code) = char c >> return code

charAscii :: Lexer Char
charAscii = choice (map parseAscii asciiMap)
  where
    parseAscii (asc, code) = try (string asc >> return code)
