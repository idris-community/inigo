module Client.Action.Clean

import Client.Action.Build
import Client.Action.BuildDeps
import Data.List
import Data.String
import Inigo.Async.Base
import Inigo.Async.Promise
import Inigo.Async.FS
import Inigo.Package.Package

cleanIPkg : String -> Promise ()
cleanIPkg ipkg =
  ignore $ system "idris2" ["--clean", ipkg] Nothing False True

export
clean : Bool -> Promise ()
clean deps =
  do
    ignore writeIPkgFile
    ignore $ cleanIPkg iPkgFile
    when (deps && !(fs_exists depsDir)) $
      do
        files <- fs_getFilesR depsDir
        let ipkgs = filter (isSuffixOf ".ipkg") files
        ignore $ all $ map cleanIPkg ipkgs
