module Client.Action.Push

import Data.Maybe
import Client.Server
import Client.Util
import Data.Buffer
import Extra.Buffer
import Inigo.Async.Archive
import Inigo.Async.Fetch
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Package.Package
import Inigo.Async.Package
import Inigo.Util.Url.Url
import Toml
import System.Path

pushArchive : Server -> String -> Package -> String -> Promise ()
pushArchive server session pkg rootPath =
  do
    contents <- Inigo.Async.Archive.buildArchive pkg rootPath
    let url = toString (fromHostPath (host server) (postArchiveUrl pkg))
    assertOk url $ request url "POST" contents [auth session]
    pure ()

pushReadme : Server -> String -> Package -> String -> Promise ()
pushReadme server session pkg contents =
  do
    let url = toString (fromHostPath (host server) (postReadmeUrl pkg))
    assertOk url $ request url "POST" contents [auth session]
    pure ()

pushPackage : Server -> String -> Package -> Promise ()
pushPackage server session pkg =
  do
    let url = toString (fromHostPath (host server) (postPackageUrl pkg))
    assertOk url $ request url "POST" (encode $ toToml pkg) [auth session]
    pure ()

maybePushReadme : Server -> String -> Maybe String -> Package -> Promise ()
maybePushReadme _ _ Nothing pkg = pure ()
maybePushReadme server session (Just path) pkg =
  do
    contents <- fs_readFile path
    pushReadme server session pkg contents

export
push : Server -> String -> String -> Promise ()
push server session packageFile =
  do
    pkg <- readPackage packageFile
    let rootPath = fromMaybe "" $ parent packageFile    
    pushArchive server session pkg rootPath
    maybePushReadme server session (map (rootPath </>) (readme pkg)) pkg
    pushPackage server session pkg
