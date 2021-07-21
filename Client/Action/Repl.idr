module Client.Action.Repl

import Inigo.Async.Base
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Package.CodeGen
import Inigo.Package.Package
import Inigo.Async.Package
import Inigo.Paths

writeIPkgNoExe : Promise ()
writeIPkgNoExe = do
    pkg <- currPackage
    fs_writeFile inigoIPkgPath (generateIPkg False Nothing pkg)

export
repl : Promise ()
repl = do
    writeIPkgNoExe
    ignore $ systemWithStdIO "idris2" ["--repl", inigoIPkgPath] True True
