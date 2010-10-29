{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Exception (load) where

import Atomo


load :: VM ()
load = do
    [$p|raise: v|] =: here "v" >>= throwError . Error

    [$p|(action: Block) catch: (recover: Block)|] =:
        catchError (eval [$e|action call|]) $ \err -> do
            modify $ \s -> s { stack = [] }

            recover <- here "recover"
            dispatch (keyword ["call"] [recover, list [asValue err]])

    [$p|(action: Block) catch: (recover: Block) ensuring: (cleanup: Block)|] =:
        catchError
            (do r <- eval [$e|action call|]
                eval [$e|cleanup call|]
                return r) $ \err -> do
            modify $ \s -> s { stack = [] }

            recover <- here "recover"

            res <- dispatch (keyword ["call"] [recover, list [asValue err]])
            eval [$e|cleanup call|]
            return res

    [$p|(action: Block) ensuring: (cleanup: Block)|] =:
        catchError
            (do r <- eval [$e|action call|]
                eval [$e|cleanup call|]
                return r)
            (\err -> eval [$e|cleanup call|] >> throwError err)



asValue :: AtomoError -> Value
asValue (Error v) = v
asValue (ParseError pe) =
    keyParticleN ["parse-error"] [string (show pe)]
asValue (DidNotUnderstand m) =
    keyParticleN ["did-not-understand"] [Message m]
asValue (Mismatch pat v) =
    keyParticleN
        ["pattern", "did-not-match"]
        [Pattern pat, v]
asValue (ImportError ie) =
    keyParticleN ["import-error"] [string (show ie)]
asValue (FileNotFound fn) =
    keyParticleN ["file-not-found"] [string fn]
asValue (ParticleArity e' g) =
    keyParticleN
        ["particle-needed", "given"]
        [Integer (fromIntegral e'), Integer (fromIntegral g)]
asValue (BlockArity e' g) =
    keyParticleN
        ["block-expected", "given"]
        [Integer (fromIntegral e'), Integer (fromIntegral g)]
asValue NoExpressions = particle "no-expressions"
asValue (ValueNotFound d v) =
    keyParticleN ["could-not-found", "in"] [string d, v]
asValue (DynamicNeeded t) =
    keyParticleN ["dynamic-needed"] [string t]
