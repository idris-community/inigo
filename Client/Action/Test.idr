module Client.Action.Test

import Client.Action.Build as Build
import Fmt
import Inigo.Async.Base
import Inigo.Async.Promise
import Inigo.Package.CodeGen as CodeGen
import Inigo.Package.Package

-- TODO: Consider moving this out from Inigo into toml config
export
test : CodeGen -> Promise ()
test codeGen =
  do
    pkg <- Build.writeIPkgFile
    log (fmt "Running tests...")
    system "idris2" ["--find-ipkg", "Test/Suite.idr", "--cg", CodeGen.toString codeGen, "-x", "suite"] True True
    pure ()
