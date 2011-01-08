{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Particle (load) where

import Atomo
import Atomo.Valuable


load :: VM ()
load = do
    [$p|(p: Particle) new: (name: String)|] =: do
        n <- getString [$e|name|]
        return (particle n)

    [$p|(p: Particle) new: (names: List)|] =: do
        ns <- liftM (map (fromText . fromString)) $ getList [$e|names|]
        return (keyParticle ns (replicate (length ns + 1) Nothing))

    [$p|(p: Particle) call: (targets: List)|] =:::
        [$e|(p complete: targets) dispatch|]

    [$p|(p: Particle) name|] =: do
        Particle (Single { mName = n }) <- here "p" >>= findParticle
        return (string n)

    [$p|(p: Particle) names|] =: do
        Particle (Keyword { mNames = ns }) <- here "p" >>= findParticle
        return $ list (map string ns)

    [$p|(p: Particle) values|] =: do
        (Particle (Keyword { mTargets = mvs })) <- here "p" >>= findParticle
        liftM list (mapM toValue mvs)

    [$p|(p: Particle) optionals|] =: do
        Particle p <- here "p" >>= findParticle
        liftM list $
            mapM (\(Option _ n mv) -> toValue (particle n, mv)) (mOptionals p)

    [$p|(p: Particle) type|] =: do
        Particle p <- here "p" >>= findParticle
        case p of
            Keyword {} -> return (particle "keyword")
            Single {} -> return (particle "single")

    [$p|(p: Particle) complete: (targets: List)|] =: do
        Particle p <- here "p" >>= findParticle
        vs <- getList [$e|targets|]

        case p of
            Keyword { mNames = ns, mTargets = mvs } ->
                let blanks = length (filter (== Nothing) mvs)
                in
                    if blanks > length vs
                        then throwError (ParticleArity blanks (length vs))
                        else return . Message . keyword ns $ completeKP mvs vs
            Single { mName = n } ->
                if null vs
                    then throwError (ParticleArity 1 0)
                    else return . Message . single n $ head vs

    [$p|c define: (p: Particle) on: v with: (targets: List) as: e|] =: do
        Particle p <- here "p" >>= findParticle
        v <- here "v"
        ts <- getList [$e|targets|]
        e <- here "e"
        c <- here "c"

        let toPattern (Pattern p) = p
            toPattern v = PMatch v
            
            others = map toPattern ts
            
            main = toPattern v

        ids <- gets primitives
        obj <- targets' ids main

        pat <-
            matchable $
                case p of
                    Keyword { mNames = ns } ->
                        keyword ns (main:others)
                    Single { mName = n } ->
                        single n main

        let m =
                case e of
                    Expression e' -> Responder pat c e'
                    _ -> Slot pat v

        forM_ obj $ \o ->
            defineOn o m
        
        return (particle "ok")

    [$p|c define: (p: Particle) on: (targets: List) as: v|] =: do
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
                    _ -> EPrimitive Nothing v

        withTop c $ do
            case p of
                Keyword { mNames = ns } ->
                    define (keyword ns targets) expr

                Single { mName = n } ->
                    define (single n (head targets)) expr

            return (particle "ok")
