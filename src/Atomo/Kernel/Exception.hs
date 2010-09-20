{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Exception (load) where

import qualified Data.IntMap as M

import Atomo.Environment
import Atomo.Haskell
import Atomo.Method


load :: VM ()
load = do
    [$p|raise: v|] =: do
        v <- here "v"
        throwError (ValueError v)

    [$p|(action: Block) catch: (recover: Block)|] =:
        catchError (eval [$e|action call|]) $ \err -> do
            recover <- here "recover"
            exc <- asValue err
            args <- list [exc]

            dispatch (keyword ["call"] [recover, args])

    [$p|(action: Block) catch: (recover: Block) ensuring: (cleanup: Block)|] =:
        catchError (eval [$e|action call|]) $ \err -> do
            recover <- here "recover"
            exc <- asValue err
            args <- list [exc]

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



asValue :: AtomoError -> VM Value
asValue (ErrorMsg s) = return (string s)
asValue (ParseError pe) = do
    obj <- lift $ gets (idObject . primitives)
    newObject $ \o -> o
        { oMethods = (toMethods
            [ (psingle "type" PSelf, particle "parse")
            , (psingle "message" PSelf, string (show pe))
            ], M.empty)
        , oDelegates = [Reference obj]
        }
asValue (DidNotUnderstand m) = do
    obj <- lift $ gets (idObject . primitives)
    newObject $ \o -> o
        { oMethods = (toMethods
            [ (psingle "type" PSelf, particle "did-not-understand")
            , (psingle "message" PSelf, Message m)
            ], M.empty)
        , oDelegates = [Reference obj]
        }
asValue (Mismatch pat v) = do
    obj <- lift $ gets (idObject . primitives)
    newObject $ \o -> o
        { oMethods = (toMethods
            [ (psingle "type" PSelf, particle "mismatch")
            , (psingle "pattern" PSelf, Pattern pat)
            , (psingle "value" PSelf, v)
            ], M.empty)
        , oDelegates = [Reference obj]
        }
asValue (ImportError ie) = do
    obj <- lift $ gets (idObject . primitives)
    newObject $ \o -> o
        { oMethods = (toMethods
            [ (psingle "type" PSelf, particle "import")
            , (psingle "message" PSelf, string (show ie))
            ], M.empty)
        , oDelegates = [Reference obj]
        }
asValue (ValueError v) = return v
