{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Pretty (load) where

import Text.PrettyPrint

import Atomo as A
import Atomo.Pretty


load :: VM ()
load = do
    ([$p|Pretty|] =::) =<< eval [$e|Object clone|]

    [$p|(o: Object) pretty|] =:
        here "o" >>= mkPretty . pretty

    [$p|(p: -> Pretty) pretty|] =::: [$e|p|]

    -- Converting values to documents
    [$p|Pretty char: (c: Char)|] =:
        here "c" >>= findChar >>= mkPretty . char . fromChar

    [$p|Pretty text: (s: String)|] =:
        getString [$e|s|] >>= mkPretty . text

    [$p|Pretty zero-width-text: (s: String)|] =:
        getString [$e|s|] >>= mkPretty . zeroWidthText

    [$p|Pretty int: (i: Integer)|] =:
        here "i" >>= findInteger >>= mkPretty . integer . A.fromInteger

    [$p|Pretty integer: (i: Integer)|] =:
        here "i" >>= findInteger >>= mkPretty . integer . A.fromInteger

    [$p|Pretty float: (d: Double)|] =:
        here "d" >>= findDouble >>= mkPretty . double . fromDouble

    [$p|Pretty double: (d: Double)|] =:
        here "d" >>= findDouble >>= mkPretty . double . fromDouble

    [$p|Pretty rational: (r: Rational)|] =:
        here "r" >>= findRational
            >>= mkPretty . rational . (\(Rational r) -> r)

    -- Simple derived documents
    [$p|Pretty semi|] =: mkPretty semi
    [$p|Pretty comma|] =: mkPretty comma
    [$p|Pretty colon|] =: mkPretty colon
    [$p|Pretty space|] =: mkPretty space
    [$p|Pretty equals|] =: mkPretty equals
    [$p|Pretty lparen|] =: mkPretty lparen
    [$p|Pretty rparen|] =: mkPretty rparen
    [$p|Pretty lbrack|] =: mkPretty lbrack
    [$p|Pretty rbrack|] =: mkPretty rbrack
    [$p|Pretty lbrace|] =: mkPretty lbrace
    [$p|Pretty rbrace|] =: mkPretty rbrace

    -- Wrapping documents in delimiters
    [$p|Pretty parens: (p: Pretty)|] =:
        here "p" >>= fromPretty >>= mkPretty . parens

    [$p|Pretty brackets: (p: Pretty)|] =:
        here "p" >>= fromPretty >>= mkPretty . brackets

    [$p|Pretty braces: (p: Pretty)|] =:
        here "p" >>= fromPretty >>= mkPretty . braces

    [$p|Pretty quotes: (p: Pretty)|] =:
        here "p" >>= fromPretty >>= mkPretty . quotes

    [$p|Pretty double-quotes: (p: Pretty)|] =:
        here "p" >>= fromPretty >>= mkPretty . doubleQuotes

    -- Combining documents
    [$p|Pretty empty|] =: mkPretty empty

    [$p|(a: Pretty) <> (b: Pretty)|] =: do
        liftM2 (<>) (here "a" >>= fromPretty) (here "b" >>= fromPretty)
            >>= mkPretty

    [$p|(a: Pretty) <+> (b: Pretty)|] =: do
        liftM2 (<+>) (here "a" >>= fromPretty) (here "b" >>= fromPretty)
            >>= mkPretty

    [$p|Pretty hcat: (ps: List)|] =: do
        getList [$e|ps|] >>= mapM fromPretty >>= mkPretty . hcat

    [$p|Pretty hsep: (ps: List)|] =: do
        getList [$e|ps|] >>= mapM fromPretty >>= mkPretty . hsep

    [$p|(a: Pretty) \\ (b: Pretty)|] =: do
        liftM2 ($$) (here "a" >>= fromPretty) (here "b" >>= fromPretty)
            >>= mkPretty

    [$p|(a: Pretty) \+\ (b: Pretty)|] =: do
        liftM2 ($+$) (here "a" >>= fromPretty) (here "b" >>= fromPretty)
            >>= mkPretty

    [$p|Pretty vcat: (ps: List)|] =: do
        getList [$e|ps|] >>= mapM fromPretty >>= mkPretty . vcat

    [$p|Pretty sep: (ps: List)|] =: do
        getList [$e|ps|] >>= mapM fromPretty >>= mkPretty . sep

    [$p|Pretty cat: (ps: List)|] =: do
        getList [$e|ps|] >>= mapM fromPretty >>= mkPretty . cat

    [$p|Pretty fsep: (ps: List)|] =: do
        getList [$e|ps|] >>= mapM fromPretty >>= mkPretty . fsep

    [$p|Pretty fcat: (ps: List)|] =: do
        getList [$e|ps|] >>= mapM fromPretty >>= mkPretty . fcat

    [$p|(p: Pretty) nest: (i: Integer)|] =: do
        d <- here "p" >>= fromPretty
        i <- here "i" >>= liftM (fromIntegral . A.fromInteger) . findInteger
        mkPretty (nest i d)

    [$p|(a: Pretty) hang: (b: Pretty) indented: (i: Integer)|] =: do
        a <- here "a" >>= fromPretty
        b <- here "b" >>= fromPretty
        i <- here "i" >>= liftM (fromIntegral . A.fromInteger) . findInteger
        mkPretty (hang a i b)

    [$p|(delimiter: Pretty) punctuate: (ps: List)|] =: do
        d <- here "delimiter" >>= fromPretty
        ps <- getList [$e|ps|] >>= mapM fromPretty
        liftM list (mapM mkPretty (punctuate d ps))

    -- Predicates on documents
    [$p|(p: Pretty) empty?|] =:
        liftM (Boolean . isEmpty) (here "p" >>= fromPretty)

    -- Rendering documents
    [$p|(p: -> Pretty) render &mode: @page &line-length: 100 &ribbons-per-line: 1.5|] =: do
        d <- here "p" >>= fromPretty
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
  where
    mkPretty :: Prettied -> VM Value
    mkPretty d =
        [$e|Pretty|] `newWith` [("doc", haskell d)]

    fromPretty :: Value -> VM Prettied
    fromPretty v = dispatch (single "doc" v) >>= fromHaskell
