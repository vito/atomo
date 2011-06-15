{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Format where

import Control.Monad.RWS
import Text.Parsec (runParser)
import Text.PrettyPrint

import Atomo
import Atomo.Format
import Atomo.Format.Parser
import Atomo.Format.Types
import Atomo.Pretty


load :: VM ()
load = do
    ([p|Formatter|] =::) =<< eval [e|Object clone|]

    [p|Formatter new: (s: String)|] =: do
        s <- getString [e|s|]
        case runParser parser (FParserState [] []) "<new:>" s of
            Right fs ->
                [e|Formatter|] `newWith`
                    [ ("format", haskell fs)
                    ]
            Left er ->
                raise ["formatting-parse"] [string (show er)]

    [p|(f: Formatter) % (... inputs)|] =: do
        fs <- eval [e|f format|] >>= fromHaskell
        is <- getList [e|inputs|]
        (_, f) <- evalRWST format fs (startState is)
        return (String f)

    [p|(f: Formatter) pretty|] =: do
        fs <- eval [e|f format|] >>= fromHaskell
        [e|Pretty|] `newWith`
            [ ("doc", haskell (char 'f' <> doubleQuotes (pretty (fs :: Format))))
            ]