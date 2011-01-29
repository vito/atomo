{-# LANGUAGE OverloadedStrings #-}
module Atomo.Format where

import Control.Arrow
import Control.Monad.Identity
import Control.Monad.RWS
import Data.Char (intToDigit)
import Numeric
import qualified Data.Text as T

import Atomo hiding (p, e)
import Atomo.Format.Types


format :: Formatter ()
format = do
    fs <- ask
    case fs of
        [] -> return ()
        ((SBreak, _) : _) -> do
            is <- inputs
            if null is
                then return ()
                else local tail format
        (f : _) -> do
            process f
            local tail format

process :: Flagged -> Formatter ()
process (SChunk s, _) = tell s
process (SString, ms) = do
    String s <- input >>= lift . findString
    justifyL ms s >>= tell
process (SInteger, ms) = integer ms showInt
process (SHex, ms) = integer ms showHex
process (SOctal, ms) = integer ms showOct
process (SBinary, ms) = integer ms (showIntAtBase 2 intToDigit)
process (SRadix, ms) = do
    n <- orIntegerInput (fPrecision ms)
    integer ms (showIntAtBase (fromIntegral n) intToDigit)
process (SFloat, ms) = float ms showFFloat
process (SExponent, ms) = float ms showEFloat
process (SGeneral, ms) = float ms showGFloat
process (SChar, ms) = do
    Char c <- input >>= lift . findChar
    justifyL ms (T.singleton c) >>= tell
process (SAsString, ms) = do
    s <- lift (here "String")
    i <- input
    String x <- lift (dispatch (keyword ["as"] [i, s]) >>= findString)
    justifyL ms x >>= tell
process (SAny, ms) = do
    i <- input
    String x <- lift (dispatch (single "show" i) >>= findString)
    justifyL ms x >>= tell
process (SPluralize fs mp, ms) = do
    w <- censor (const "") (liftM snd (listen (with fs format)))
    Integer n <-
        if fSymbol ms '>'
            then liftM head inputs >>= lift . findInteger
            else input >>= lift . findInteger

    if n == 1
        then tell w
        else do
        
    case mp of
        Nothing -> do
            String p <- lift (dispatch (single "plural" (String w)) >>= findString)
            tell p
        Just p ->
            with p format
process (SLowercase fs, _) =
    censor T.toLower (with fs format)
process (SCapitalize fs, _) =
    censor cap (with fs format)
  where
    cap t = T.toUpper (T.take 1 t) `T.append` (T.drop 1 t)
process (SUppercase fs, _) =
    censor T.toUpper (with fs format)
process (SSkip, ms) = do
    n <- liftM (maybe 1 id) (fNumber ms)
    modify (second (+ (if back then -n else n)))
  where
    back = fSymbol ms '<'
process (SIndirection, ms) = do
    f <- input
    fs <- lift (dispatch (single "format" f) >>= fromHaskell)
    if fSymbol ms '*'
        then with fs format
        else do
    
    is <- input >>= liftM fromList . (lift . findList)
    old <- get
    put (is, 0)
    with fs format
    put old
process (SIterate fs, ms) = do
    let rest = fSymbol ms '*'

    n <- fNumber ms
    is <-
        if rest
            then get
            else do
                i <- input >>= lift . findList
                return (fromList i, 0)

    ois <- get
    ins <- inputs

    let sub = fSymbol ms '.'
        alwaysRun = fSymbol ms '+'

    if null ins && alwaysRun && n /= Just 0
        then with fs format
        else do

    case n of
        Nothing | sub -> do
            forM_ ins $ \i -> do
                put (fromList i, 0)
                with fs format
        Nothing -> do
            put is
            iter
        Just m -> do
            put is
            iterMax m

    if rest
        then modify (second (const (length ins)))
        else put ois
  where
    iter = do
        is <- inputs
        case is of
            [] -> return ()
            _ -> do
                with fs format
                iter

    iterMax 0 = return ()
    iterMax n = do
        is <- inputs
        case is of
            [] -> return ()
            _ -> do
                with fs format
                iterMax (n - 1)
process (SBreak, _) = return ()
process (SConditional fss md, ms) = do
    case (fSymbol ms '?', fss) of
        (True, (t:f:_)) -> do
            Boolean b <- input >>= lift . findBoolean
            with (if b then t else f) format
        (True, (t:_)) -> do
            Boolean b <- input >>= lift . findBoolean
            if b
                then with t format
                else return ()
        _ -> do
            i <- fNumber ms >>= orIntegerInput
            if i >= length fss
                then maybe (return ()) (flip with format) md
                else with (fss !! i) format
process (SJustify fss, ms) = do
    ts <- forM fss $ \fs -> censor (const "") $ do
        (_, o) <- listen (with fs format)
        return o
    
    justify ms ts >>= tell

input :: Formatter Value
input = do
    i <- inputs
    case i of
        [] -> lift (raise' "incomplete-input")
        (x:_) -> do
            modify (second succ)
            return x

inputs :: Formatter [Value]
inputs = do
    i <- get
    return (drop (snd i) (fst i))

orIntegerInput :: Maybe Int -> Formatter Int
orIntegerInput =
    maybe
        (liftM (\(Integer i) -> fromIntegral i) $
            input >>= lift . findInteger)
        return

integer :: [Flag] -> (Integer -> String -> String) -> Formatter ()
integer ms f = do
    Integer v <- input >>= lift . findInteger
    justifyR ms (T.pack $ f v "") >>= tell

float :: [Flag] -> (Maybe Int -> Double -> String -> String) -> Formatter ()
float ms f = do
    Double v <- input >>= lift . findDouble
    justifyR ms (T.pack $ f prec v "") >>= tell
  where
    prec = fPrecision ms

justifyL :: [Flag] -> T.Text -> Formatter T.Text
justifyL fs s = do
    n <- fNumber fs
    return $
        case n of
            Nothing -> s
            Just w | fSymbol fs '=' ->
                T.center w pad s
            Just w | fSymbol fs '>' ->
                T.justifyRight w pad s
            Just w ->
                T.justifyLeft w pad s
  where
    pad | FZeroPad `elem` fs = '0'
        | otherwise = ' '

justifyR :: [Flag] -> T.Text -> Formatter T.Text
justifyR fs s = do
    n <- fNumber fs
    return $
        case n of
            Nothing -> s
            Just w | fSymbol fs '=' ->
                T.center w pad s
            Just w | fSymbol fs '<' ->
                T.justifyLeft w pad s
            Just w ->
                T.justifyRight w pad s
  where
    pad | FZeroPad `elem` fs = '0'
        | otherwise = ' '

justify :: [Flag] -> [T.Text] -> Formatter T.Text
justify fs [t] = justifyR fs t
justify fs ts = do
    n <- fNumber fs
    return $
        case n of
            Nothing -> T.concat ts
            Just w -> justifyTo w ts

justifyTo :: Int -> [T.Text] -> T.Text
justifyTo to ts = done ts
  where
    need = to - sum (map T.length ts)
    naiveAvg = need `div` (length ts - 1)

    -- special case; e.g. 5 `div` 3 is 1, so we end up with 1|1|3;
    -- try turning that into 1|2|2
    --
    -- to determine this, we see if the leftover space could be reduced
    -- by adding 1 to the other spacings
    avg | (need - naiveAvg * (length ts - 1)) >= (length ts - 2) =
            naiveAvg + 1
        | otherwise = naiveAvg

    done [] = T.empty
    done (w:ws) = T.concat
        [ w
        , space naiveAvg
        , spaced (need - naiveAvg) ws
        ]

    spaced _ [] = T.empty
    spaced n [w] = space n `T.append` w
    spaced n (w:ws) = T.concat
        [ w
        , space avg
        , spaced (n - avg) ws
        ]

    space = flip T.replicate (T.singleton ' ')

fNumber :: [Flag] -> Formatter (Maybe Int)
fNumber [] = return Nothing
fNumber (FNumber (Just n):_) = return (Just n)
fNumber (FNumber Nothing:_) = liftM (Just . length) inputs
fNumber (_:fs) = fNumber fs

fSymbol :: [Flag] -> Char -> Bool
fSymbol [] _ = False
fSymbol (FSymbol x:_) c | x == c = True
fSymbol (_:fs) c = fSymbol fs c

fPrecision :: [Flag] -> Maybe Int
fPrecision [] = Nothing
fPrecision (FPrecision n:_) = return n
fPrecision (_:fs) = fPrecision fs

with :: [Flagged] -> Formatter a -> Formatter a
with fs = local (const fs)
