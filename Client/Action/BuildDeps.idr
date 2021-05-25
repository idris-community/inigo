module Client.Action.BuildDeps

import Data.List
import Data.String
import Fmt
import Inigo.Async.Base
import Inigo.Async.FS
import Inigo.Async.Promise

depsDir : String
depsDir = "./Deps"

buildIPkg : String -> Promise ()
buildIPkg ipkg =
  do
    log (fmt "Compiling %s" ipkg)
    ignore $ system "idris2" ["--build", ipkg] False True
    log (fmt "Compiled %s" ipkg)

export
buildDeps : Promise ()
buildDeps =  
  do
    files <- fs_getFilesR depsDir
    let ipkgs = filter (isSuffixOf ".ipkg") files
    ignore $ all $ map buildIPkg ipkgs
