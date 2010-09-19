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
        -- name -> [package]
        packages = []

        -- versions loaded by @use:
        -- name -> package
        loaded = []
    }

    Eco Package = Object clone do: {
        version = 0 . 1
        dependencies = []
        include = []
        executables = []
    }

    (e: Eco) initialize :=
        e initialize: (Eco Package load-from: "package.eco")

    (e: Eco) initialize: pkg := {
        pkg dependencies each: { c |
            e packages (find: c from) match: {
                @none -> raise: ("required package not found: " .. c from .. " (satisfying " .. c to show .. ")")

                -- filter out versions not satisfying our dependency
                @(ok: d) -> {
                    safe = d to (filter: { p | p version join: c to })

                    safe match: {
                        [] ->
                            raise:
                                [
                                    "no versions of package "
                                    d from
                                    " (" .. d to (map: { p | p version as: String }) (join: ", ") .. ")"
                                    " satisfy constraint "
                                    c to show
                                    " for package " .. pkg name show
                                ] concat

                        -- initialize the first safe version of the package
                        (p . _) -> {
                            -- remove unsafe versions from the ecosystem
                            d to = safe
                            e initialize: p
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
            pkgs = versions map: { v |
                Eco Package load-from:
                    (eco </> c </> v </> "package.eco")
            }

            c -> pkgs (sort-by: { a b | a version < b version })
        }
    } call

    Eco path-to: (c: Eco Package) :=
        Eco path-to: c name version: c version

    Eco path-to: (name: String) :=
        Directory home </> ".eco" </> "lib" </> name

    Eco path-to: (name: String) version: (v: Version) :=
        (Eco path-to: name) </> (v as: String)

    Eco executable: (name: String) :=
        Directory home </> ".eco" </> "bin" </> name

    Eco install: (path: String) := {
        pkg = Eco Package load-from: (path </> "package.eco")
        target = Eco path-to: pkg

        contents = "main.atomo" . ("package.eco" . pkg include)

        Directory create-tree-if-missing: target

        contents each: { c |
            if: Directory (exists?: (path </> c))
                then: { Directory copy: (path </> c) to: (target </> c) }
                else: { File copy: (path </> c) to: (target </> c) }
        }

        pkg executables each: { e |
            File copy: (path </> e to) to: (Eco executable: e from)
            File set-executable: (Eco executable: e from) to: True
        }
    } call

    Eco uninstall: (name: String) version: (version: Version) := {
        path = Eco path-to: name version: version
        pkg = Eco Package load-from: (path </> "package.eco")

        Directory remove-recursive: path
        pkg executables each: { e |
            File remove: (Eco executable: e from)
        }
    } call

    Eco uninstall: (name: String) :=
        Directory (contents: (Eco path-to: name)) each: { v |
            Eco uninstall: name version: (v as: Version)
        }

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
                    @(ok: pkgs) -> {
                        satisfactory = pkgs filter: { p | p version join: check }

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

                        context load: ((Eco path-to: satisfactory head) </> "main.atomo")
                    } call
                }
            } call

            @(ok: p) ->
                when: (p version join: check) not
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
