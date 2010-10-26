{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Particle (load) where

import Atomo


load :: VM ()
load = do
    [$p|(p: Particle) call: (targets: List)|] =:::
        [$e|(p complete: targets) send|]

    [$p|(p: Particle) name|] =: do
        Particle (PMSingle n) <- here "p" >>= findParticle
        return (string n)

    [$p|(p: Particle) names|] =: do
        Particle (PMKeyword ns _) <- here "p" >>= findParticle
        return $ list (map string ns)

    [$p|(p: Particle) values|] =: do
        (Particle (PMKeyword _ mvs)) <- here "p" >>= findParticle
        return . list $
            map
                (maybe (particle "none") (keyParticleN ["ok"] . (:[])))
                mvs

    [$p|(p: Particle) type|] =: do
        Particle p <- here "p" >>= findParticle
        case p of
            PMKeyword {} -> return (particle "keyword")
            PMSingle {} -> return (particle "single")

    [$p|(p: Particle) complete: (targets: List)|] =: do
        Particle p <- here "p" >>= findParticle
        vs <- getList [$e|targets|]

        case p of
            PMKeyword ns mvs -> do
                let blanks = length (filter (== Nothing) mvs)

                if blanks > length vs
                    then throwError (ParticleArity blanks (length vs))
                    else return . Message . keyword ns $ completeKP mvs vs
            PMSingle n -> do
                if length vs == 0
                    then throwError (ParticleArity 1 0)
                    else return . Message . single n $ head vs

    [$p|(p: Particle) define-on: (targets: List) as: v|] =:::
        [$e|p define-on: targets as: v in: sender|]

    [$p|(p: Particle) define-on: (targets: List) as: v in: c|] =: do
        Particle p <- here "p" >>= findParticle
        vs <- getList [$e|targets|]
        v <- here "v"
        c <- here "c"

        let targets =
                map (\v ->
                    case v of
                        Pattern p -> p
                        _ -> PMatch v) vs
            expr =
                case v of
                    Expression e -> e
                    _ -> Primitive Nothing v

        withTop c $ do
            case p of
                PMKeyword ns _ ->
                    define (pkeyword ns targets) expr

                PMSingle n ->
                    define (psingle n (head targets)) expr

            return (particle "ok")
