{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Exception (load) where

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    [$p|raise: v|] =: do
        v <- here "v"
        throwError (Error v)

    [$p|(action: Block) catch: (recover: Block)|] =:
        catchError (eval [$e|action call|]) $ \err -> do
            recover <- here "recover"
            args <- list [asValue err]

            dispatch (keyword ["call"] [recover, args])

    [$p|(action: Block) catch: (recover: Block) ensuring: (cleanup: Block)|] =:
        catchError
            (do r <- eval [$e|action call|]
                eval [$e|cleanup call|]
                return r) $ \err -> do
            recover <- here "recover"
            args <- list [asValue err]

            res <- dispatch (keyword ["call"] [recover, args])
            eval [$e|cleanup call|]
            return res

    [$p|(action: Block) handle: (branches: Block)|] =::: [$e|
        action catch: { e |
            { e match: branches } catch: { e* |
                e* match: {
                    @(no-matches-for: _) -> raise: e
                    _ -> raise: e*
                }
            }
        }
    |]

    [$p|(a: Block) handle: (b: Block) ensuring: (c: Block)|] =::: [$e|
        { a handle: b } ensuring: c
    |]

    [$p|(action: Block) ensuring: (cleanup: Block)|] =:
        catchError
            (do r <- eval [$e|action call|]
                eval [$e|cleanup call|]
                return r)
            (\err -> eval [$e|cleanup call|] >> throwError err)

    [$p|v ensuring: p do: b|] =:::
        [$e|{ b call: [v] } ensuring: { p call: [v] }|]



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
