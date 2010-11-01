{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Exception (load) where

import Atomo


load :: VM ()
load = do
    [$p|raise: v|] =: here "v" >>= throwError . Error
