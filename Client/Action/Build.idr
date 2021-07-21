module Client.Action.Build

import Inigo.Async.Base
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Package.CodeGen
import Inigo.Package.Package
import Inigo.Async.Package
import Inigo.Paths

export
writeIPkgFile : Promise Package
writeIPkgFile =
  do
    pkg <- Inigo.Async.Package.currPackage
    -- TODO: Only build if not exists ?
    fs_writeFile inigoIPkgPath (Package.Package.generateIPkg True Nothing pkg)
    pure pkg

export
runBuild : CodeGen -> Promise ()
runBuild codeGen =
    ignore $ system "idris2" ["--build", inigoIPkgPath, "--cg", toString codeGen] Nothing False True

export
build : CodeGen -> Promise ()
build codeGen =
  do
    pkg <- writeIPkgFile
    runBuild codeGen
