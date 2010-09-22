{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Particle (load) where

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    [$p|(p: Particle) call: (l: List)|] =: do
        Particle p <- here "p" >>= findValue isParticle
        vs <- getList [$e|l|]

        case p of
            PMKeyword ns mvs -> do
                let blanks = length (filter (== Nothing) mvs)

                if blanks > length vs
                    then throwError (ParticleArity blanks (length vs))
                    else dispatch (keyword ns $ completeKP mvs vs)
            PMSingle n -> do
                if length vs == 0
                    then throwError (ParticleArity 1 0)
                    else dispatch (single n (head vs))

    [$p|(p: Particle) name|] =: do
        Particle (PMSingle n) <- here "p" >>= findValue isParticle
        return (string n)

    [$p|(p: Particle) names|] =: do
        Particle (PMKeyword ns _) <- here "p" >>= findValue isParticle
        list (map string ns)

    [$p|(p: Particle) values|] =: do
        (Particle (PMKeyword _ mvs)) <- here "p" >>= findValue isParticle
        list $
            map
                (maybe (particle "none") (keyParticleN ["ok"] . (:[])))
                mvs

    [$p|(p: Particle) type|] =: do
        Particle p <- here "p" >>= findValue isParticle
        case p of
            PMKeyword {} -> return (particle "keyword")
            PMSingle {} -> return (particle "single")
