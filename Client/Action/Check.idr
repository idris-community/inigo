module Client.Action.Check

import Client.Action.Build
import Inigo.Async.Base
import Inigo.Async.Promise
import Inigo.Package.Package
import Inigo.Paths

export
check : Promise ()
check =
  do
    ignore writeIPkgFile
    ignore $ system "idris2" ["--typecheck", inigoIPkgPath] Nothing False False
