module Client.Action.Pull

import Client.Server
import Data.Buffer
import Extra.Buffer
import Fmt
import Inigo.Archive.Path
import Inigo.Async.Archive
import Inigo.Async.Base
import Inigo.Async.Fetch
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Package.Package
import Inigo.Util.Url.Url
import System.Path
import SemVar

getPath : String -> String -> (Maybe Version) -> String
getPath ns package Nothing = getPackageUrl ns package
getPath ns package (Just version) = getPackageVersionUrl ns package version

getPackage : Server -> String -> String -> (Maybe Version) -> Promise Package
getPackage server packageNS packageName maybeVersion =
  do
    let path = getPath packageNS packageName maybeVersion
    let url = toString (fromHostPath (host server) path)
    log ("Requesting " ++ url ++ "...")
    contents <- fetch url
    let Right package = parsePackage contents
      | Left err => reject ("Invalid package: " ++ err)
    pure package

getArchive : Server -> Package -> Promise Buffer
getArchive server pkg =
  do
    let url = toString (fromHostPath (host server) (getArchiveUrl pkg))
    log ("Requesting " ++ url ++ "...")
    contents <- fetchBuf url
    pure contents

export
pull : Server -> String -> String -> (Maybe Version) -> Promise ()
pull server packageNS packageName maybeVersion =
  do
    pkg <- getPackage server packageNS packageName maybeVersion
    archive <- getArchive server pkg
    let depPath = Archive.Path.depPath pkg
    extractArchive archive depPath
    let iPkgFile = depPath </> "Inigo.ipkg"
    log (fmt "Writing %s..." iPkgFile)
    fs_writeFile iPkgFile (Package.Package.generateIPkg False (Just "../../../../build") pkg)
