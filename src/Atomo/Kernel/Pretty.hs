{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Pretty (load) where

import Text.PrettyPrint

import Atomo as A
import Atomo.Pretty
import Atomo.Valuable


load :: VM ()
load = do
    ([p|Pretty|] =::) =<< eval [e|Object clone|]

    [p|(o: Object) pretty|] =:
        here "o" >>= toValue . pretty

    [p|(p: -> Pretty) pretty|] =::: [e|p|]

    -- Converting values to documents
    [p|Pretty char: (c: Character)|] =:
        here "c" >>= findCharacter >>= toValue . char . fromCharacter

    [p|Pretty text: (s: String)|] =:
        getString [e|s|] >>= toValue . text

    [p|Pretty zero-width-text: (s: String)|] =:
        getString [e|s|] >>= toValue . zeroWidthText

    [p|Pretty int: (i: Integer)|] =:
        here "i" >>= findInteger >>= toValue . integer . A.fromInteger

    [p|Pretty integer: (i: Integer)|] =:
        here "i" >>= findInteger >>= toValue . integer . A.fromInteger

    [p|Pretty float: (d: Double)|] =:
        here "d" >>= findDouble >>= toValue . double . fromDouble

    [p|Pretty double: (d: Double)|] =:
        here "d" >>= findDouble >>= toValue . double . fromDouble

    [p|Pretty rational: (r: Rational)|] =:
        here "r" >>= findRational
            >>= toValue . rational . (\(Rational r) -> r)

    -- Simple derived documents
    [p|Pretty semi|] =: toValue semi
    [p|Pretty comma|] =: toValue comma
    [p|Pretty colon|] =: toValue colon
    [p|Pretty space|] =: toValue space
    [p|Pretty equals|] =: toValue equals
    [p|Pretty lparen|] =: toValue lparen
    [p|Pretty rparen|] =: toValue rparen
    [p|Pretty lbrack|] =: toValue lbrack
    [p|Pretty rbrack|] =: toValue rbrack
    [p|Pretty lbrace|] =: toValue lbrace
    [p|Pretty rbrace|] =: toValue rbrace

    -- Wrapping documents in delimiters
    [p|Pretty parens: (p: Pretty)|] =:
        here "p" >>= fromValue >>= toValue . parens

    [p|Pretty brackets: (p: Pretty)|] =:
        here "p" >>= fromValue >>= toValue . brackets

    [p|Pretty braces: (p: Pretty)|] =:
        here "p" >>= fromValue >>= toValue . braces

    [p|Pretty quotes: (p: Pretty)|] =:
        here "p" >>= fromValue >>= toValue . quotes

    [p|Pretty double-quotes: (p: Pretty)|] =:
        here "p" >>= fromValue >>= toValue . doubleQuotes

    -- Combining documents
    [p|Pretty empty|] =: toValue empty

    [p|(a: Pretty) <> (b: Pretty)|] =: do
        liftM2 (<>) (here "a" >>= fromValue) (here "b" >>= fromValue)
            >>= toValue

    [p|(a: Pretty) <+> (b: Pretty)|] =: do
        liftM2 (<+>) (here "a" >>= fromValue) (here "b" >>= fromValue)
            >>= toValue

    [p|Pretty hcat: (ps: List)|] =: do
        getList [e|ps|] >>= mapM fromValue >>= toValue . hcat

    [p|Pretty hsep: (ps: List)|] =: do
        getList [e|ps|] >>= mapM fromValue >>= toValue . hsep

    [p|(a: Pretty) \\ (b: Pretty)|] =: do
        liftM2 ($$) (here "a" >>= fromValue) (here "b" >>= fromValue)
            >>= toValue

    [p|(a: Pretty) \+\ (b: Pretty)|] =: do
        liftM2 ($+$) (here "a" >>= fromValue) (here "b" >>= fromValue)
            >>= toValue

    [p|Pretty vcat: (ps: List)|] =: do
        getList [e|ps|] >>= mapM fromValue >>= toValue . vcat

    [p|Pretty sep: (ps: List)|] =: do
        getList [e|ps|] >>= mapM fromValue >>= toValue . sep

    [p|Pretty cat: (ps: List)|] =: do
        getList [e|ps|] >>= mapM fromValue >>= toValue . cat

    [p|Pretty fsep: (ps: List)|] =: do
        getList [e|ps|] >>= mapM fromValue >>= toValue . fsep

    [p|Pretty fcat: (ps: List)|] =: do
        getList [e|ps|] >>= mapM fromValue >>= toValue . fcat

    [p|(p: Pretty) nest: (i: Integer)|] =: do
        d <- here "p" >>= fromValue
        i <- here "i" >>= liftM (fromIntegral . A.fromInteger) . findInteger
        toValue (nest i d)

    [p|(a: Pretty) hang: (b: Pretty) indented: (i: Integer)|] =: do
        a <- here "a" >>= fromValue
        b <- here "b" >>= fromValue
        i <- here "i" >>= liftM (fromIntegral . A.fromInteger) . findInteger
        toValue (hang a i b)

    [p|(delimiter: Pretty) punctuate: (ps: List)|] =: do
        d <- here "delimiter" >>= fromValue
        ps <- getList [e|ps|] >>= mapM fromValue
        liftM list (mapM toValue (punctuate d ps))

    -- Predicates on documents
    [p|(p: Pretty) empty?|] =:
        liftM (Boolean . isEmpty) (here "p" >>= fromValue)

    -- Rendering documents
    [p|(p: -> Pretty) render &mode: @page &line-length: 100 &ribbons-per-line: 1.5|] =: do
        d <- here "p" >>= fromValue
        m <- here "mode" >>= findParticle
        sl <- here "line-length" >>= liftM (fromIntegral . A.fromInteger) . findInteger
        sr <- here "ribbons-per-line" >>= liftM (fromRational . toRational . fromDouble) . findDouble

        sm <-
            case m of
                Particle (Single { mName = "page" }) ->
                    return PageMode

                Particle (Single { mName = "zig-zag" }) ->
                    return ZigZagMode

                Particle (Single { mName = "left" }) ->
                    return LeftMode

                Particle (Single { mName = "one-line" }) ->
                    return OneLineMode

                _ ->
                    raise ["unknown-render-mode", "must-be"]
                        [ m
                        , list
                            [ particle "page"
                            , particle "zig-zag"
                            , particle "left"
                            , particle "one-line"
                            ]
                        ]

        return (string (renderStyle (Style sm sl sr) d))