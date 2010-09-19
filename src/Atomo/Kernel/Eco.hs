{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Eco where

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = loadVersions >> loadEco

loadEco :: VM ()
loadEco = mapM_ eval [$es|
    Eco = Object clone do: {
        -- all packages OK for loading
        -- name -> [version]
        packages = []

        -- versions loaded by @use:
        -- name -> version
        loaded = []
    }

    Eco Package = Object clone do: {
        version = 0 . 1
        dependencies = []
        include = []
        executables = []
    }

    (e: Eco) initialize := e initialize: "package.eco"

    (e: Eco) initialize: file := {
        "initializing " (.. file) print

        me = Eco Package load-from: file

        me dependencies each: { c |
            e packages (find: c from) match: {
                @none -> raise: ("required package not found: " .. c from .. " (satisfying " .. c to show .. ")")

                -- filter out versions not satisfying our dependency
                @(ok: d) -> {
                    safe = d to (filter: @(join: c to))

                    safe match: {
                        [] ->
                            raise:
                                [
                                    "no versions of package "
                                    d from
                                    " (" .. d to (map: @(as: String)) (join: ", ") .. ")"
                                    " satisfy constraint "
                                    c to show
                                    " for package " .. me name show
                                ] concat
                        (v . _) -> {
                            d to = safe
                            e initialize: (e path-to: d from version: v) </> "package" <.> "eco"
                        } call
                    }
                } call
            }
        }

        @ok
    } call

    (e: Eco) load := {
        eco = Directory home </> ".eco" </> "lib"

        e packages = Directory (contents: eco) map: { c |
            versions = Directory (contents: (eco </> c))
            c -> versions (map: @(as: Version)) (sort-by: @<)
        }
    } call

    Eco path-to: (c: Eco Package) :=
        Eco path-to: c name version: c version

    Eco path-to: (name: String) version: (v: Version) :=
        Directory home </> ".eco" </> "lib" </> name </> (v as: String)

    Eco executable: (name: String) :=
        Directory home </> ".eco" </> "bin" </> name

    context use: (name: String) :=
        context use: name version: { True }

    context use: (name: String) version: (v: Version) :=
        context use: name version: { == v }

    context use: (name: String) version: (check: Block) := {
        Eco loaded (lookup: name) match: {
            @none -> {
                Eco packages (lookup: name) match: {
                    @none -> raise: ("package not found: " .. name)
                    @(ok: []) -> raise: ("no versions for package: " .. name)
                    @(ok: versions) -> {
                        satisfactory = versions filter: @(join: check)

                        when: satisfactory empty?
                            do: {
                                raise: 
                                    [
                                        "no versions of package "
                                        name
                                        " (" .. versions (map: @(as: String)) (join: ", ") .. ")"
                                        " satisfy constraint "
                                        check show
                                    ] concat
                            }

                        Eco loaded << (name -> satisfactory head)

                        context load:
                            ((Eco path-to: name version: satisfactory head) </> "main" <.> "atomo")
                    } call
                }
            } call

            @(ok: v) ->
                when: (v join: check) not
                    do: { raise: ("package " .. name .. " already loaded, but version (" .. (v as: String) .. ") does not satisfy constraint " .. check show) }
        }

        @ok
    } call

    (p: Eco Package) name: (n: String) :=
        p name = n

    (p: Eco Package) description: (d: String) :=
        p description = d

    (p: Eco Package) version: (v: Version) :=
        p version = v

    (p: Eco Package) author: (a: String) :=
        p author = a

    (p: Eco Package) include: (l: List) :=
        p include = l

    (p: Eco Package) depends-on: (ds: List) :=
        p dependencies = ds map: { d |
            if: (d is-a?: Association)
                then: { d }
                else: { d -> { True } }
        }

    (p: Eco Package) executables: (es: List) :=
        p executables = es

    Eco Package load-from: (file: String) :=
        Eco Package clone do: { load: file }

    Eco load
|]

loadVersions :: VM ()
loadVersions = mapM_ eval [$es|
    Version = Object clone

    (major: Integer) . (minor: Integer) :=
        Version clone do: {
            major = major
            minor = minor
        }

    (major: Integer) . (rest: Version) :=
        Version clone do: {
            major = major
            minor = rest
        }

    (v: Version) show :=
        v major show .. " . " .. v minor show

    (v: Version) as: String :=
        v major show .. "." .. v minor (as: String)

    (s: String) as: Version :=
        s (split-on: '.') (map: @(as: Integer)) reduce-right: @.

    (n: Integer) as: Version := n . 0

    (a: Version) == (b: Version) :=
        (a major == b major) and: { a minor == b minor }

    (a: Version) > (b: Version) :=
        (a major > b major) or: { a minor > b minor }

    (n: Integer) > (v: Version) :=
        (n > v major) or: { v major == n && v minor /= 0 }

    (v: Version) > (n: Integer) :=
        (v major > n) or: { v major == n && v minor /= 0 }

    (a: Version) < (b: Version) :=
        (a major < b major) or: { a minor < b minor }

    (n: Integer) < (v: Version) :=
        (n < v major) or: { v major == n && v minor /= 0 }

    (v: Version) < (n: Integer) :=
        (v major < n) or: { v major == n && v minor /= 0 }

    (a: Version) >= (b: Version) :=
        (a major >= b major) or: { a minor >= b minor }

    (n: Integer) >= (v: Version) :=
        n >= v major

    (v: Version) >= (n: Integer) :=
        v major >= n

    (a: Version) <= (b: Version) :=
        (a major <= b major) or: { a minor <= b minor }

    (n: Integer) <= (v: Version) :=
        n <= v major

    (v: Version) <= (n: Integer) :=
        v major <= n
|]
