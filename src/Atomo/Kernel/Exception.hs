{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Exception (load) where

import Atomo


load :: VM ()
load = do
    [$p|raise: v|] =: here "v" >>= throwError . Error

    {-[$p|(action: Block) catch: (recover: Block)|] =:-}
        {-catchError (eval [$e|action call|]) $ \err -> do-}
            {-modify $ \s -> s { stack = [] }-}

            {-recover <- here "recover"-}
            {-dispatch (keyword ["call"] [recover, list [asValue err]])-}

    {-[$p|(action: Block) catch: (recover: Block) ensuring: (cleanup: Block)|] =:-}
        {-catchError-}
            {-(do r <- eval [$e|action call|]-}
                {-eval [$e|cleanup call|]-}
                {-return r) $ \err -> do-}
            {-modify $ \s -> s { stack = [] }-}

            {-recover <- here "recover"-}

            {-res <- dispatch (keyword ["call"] [recover, list [asValue err]])-}
            {-eval [$e|cleanup call|]-}
            {-return res-}

    {-[$p|(action: Block) ensuring: (cleanup: Block)|] =:-}
        {-catchError-}
            {-(do r <- eval [$e|action call|]-}
                {-eval [$e|cleanup call|]-}
                {-return r)-}
            {-(\err -> eval [$e|cleanup call|] >> throwError err)-}



