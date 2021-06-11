module Client.Action.BuildDeps

import Client.Action.CacheDeps
import Data.List
import Data.SortedMap as Map
import Data.SortedSet as Set
import Data.String
import Fmt
import Inigo.Async.Base
import Inigo.Async.Package
import Inigo.Async.Promise
import Inigo.Package.Package
import Inigo.Paths
import Inigo.PkgTree
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
    pkgs <- readDepCache
    pkg <- currPackage
    let ctxt = fromList pkgs
    log $ "Generating build order" ++ if dev then " with dev dependencies" else ""
    debugLog "Packages to build: \{show $ Set.toList $ keySet ctxt}"
    debugLog "Package context: \{show $ Map.toList $ getDeps {name=String} dev <$> ctxt}"
    build <- liftEither $ getBuildOrder dev ctxt (keySet ctxt)
    traverse_ buildIPkg build
