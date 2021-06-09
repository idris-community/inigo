module Client.Action.BuildDeps

import Data.List
import Data.String
import Fmt
import Inigo.Async.Base
import Inigo.Async.FS
import Inigo.Async.Package
import Inigo.Async.Promise
import Inigo.Package.Package
import SemVar
import System.Path

export
depsDir : String
depsDir = "./Deps"

buildIPkg : String -> Promise ()
buildIPkg ipkg =
  do
    log (fmt "Compiling %s" ipkg)
    ignore $ system "idris2" ["--build", ipkg] Nothing False True
    log (fmt "Compiled %s" ipkg)

export
buildDeps : Bool -> Promise ()
-- buildDeps =  
--   do
--     files <- fs_getFilesR depsDir
--     let ipkgs = filter (isSuffixOf ".ipkg") files
--     ignore $ all $ map buildIPkg ipkgs
buildDeps dev = do
    pkg <- currPackage
    let allDeps = pkg.deps ++ if dev then pkg.devDeps else []
    let depNames = map fst allDeps
    ignore $ all $ map buildDep depNames
  where
    buildDep : List String -> Promise ()
    buildDep dep = do
        let ipkg = joinPath dep </> "Inigo.ipkg"
        buildIPkg ipkg

export
buildExtraDeps : Promise ()
buildExtraDeps = do
    pkg <- currPackage
    ignore $ all $ map buildExtraDep pkg.extraDeps
  where
    buildExtraDep : ExtraDep -> Promise ()
    buildExtraDep (MkExtraDep _ _ url subFolders) = do
        let ipkgs = (\f => genFolder url </> f </> "Inigo.ipkg") <$> subFolders
        ignore $ all $ map buildIPkg ipkgs
