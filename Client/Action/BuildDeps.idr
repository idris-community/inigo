module Client.Action.BuildDeps

import Client.Action.CacheDeps
import Data.List
import Data.String
import Fmt
import Inigo.Async.Base
import Inigo.Async.FS
import Inigo.Async.Package
import Inigo.Async.Promise
import Inigo.Package.Package
import Inigo.Paths
import SemVar

buildIPkg : String -> Promise ()
buildIPkg path =
  do
    log (fmt "Compiling %s" (path </> inigoIPkgPath))
    debugLog "\{path}$ idris2 --build \{inigoIPkgPath}"
    0 <- system "idris2" ["--build", inigoIPkgPath] (Just path) False True
        | errno => reject "idris2 build error: \{show errno}"
    log (fmt "Compiled %s" (path </> inigoIPkgPath))

export
buildDeps : Bool -> Promise ()
buildDeps dev = do
    pkg <- currPackage
    let allDeps = pkg.deps ++ if dev then pkg.devDeps else []
    let depNames = map fst allDeps
    ignore $ all $ map buildDep depNames
  where
    buildDep : List String -> Promise ()
    buildDep dep = buildIPkg $ joinPath dep

export
buildExtraDeps : Promise ()
buildExtraDeps = do
    pkgs <- readDepCache
    traverse_ buildPkg pkgs -- TODO: topological ordering
  where
    buildPkg : (String, Package) -> Promise ()
    buildPkg (src, _) = buildIPkg src
