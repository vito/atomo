{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Exception (load) where

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    [$p|raise: v|] =: do
        v <- here "v"
        throwError (ValueError v)

    [$p|(action: Block) catch: (recover: Block)|] =:
        catchError (eval [$e|action call|]) $ \err -> do
            recover <- here "recover"
            args <- list [asValue err]

            dispatch (keyword ["call"] [recover, args])

    [$p|(action: Block) catch: (recover: Block) ensuring: (cleanup: Block)|] =:
        catchError (eval [$e|action call|]) $ \err -> do
            recover <- here "recover"
            args <- list [asValue err]

            res <- dispatch (keyword ["call"] [recover, args])
            eval [$e|cleanup call|]
            return res

    [$p|(action: Block) ensuring: (cleanup: Block)|] =:
        catchError
            (do r <- eval [$e|action call|]
                eval [$e|cleanup call|]
                return r)
            (\err -> eval [$e|cleanup call|] >> throwError err)

    [$p|v ensuring: p do: b|] =:::
        [$e|{ b call: [v] } ensuring: { p call: [v] }|]



asValue :: AtomoError -> Value
asValue (ErrorMsg s) = keyParticle ["error"] [Nothing, Just (string s)]
asValue (ParseError pe) =
    keyParticle ["parse-error"] [Nothing, Just (string (show pe))]
asValue (DidNotUnderstand m) =
    keyParticle ["did-not-understand"] [Nothing, Just (Message m)]
asValue (Mismatch pat v) =
    keyParticle
        ["pattern", "did-not-match"]
        [Nothing, Just (Pattern pat), Just v]
asValue (ImportError ie) =
    keyParticle ["import-error"] [Nothing, Just (string (show ie))]
asValue (FileNotFound fn) =
    keyParticle ["file-not-found"] [Nothing, Just (string fn)]
asValue (ValueError v) = v
