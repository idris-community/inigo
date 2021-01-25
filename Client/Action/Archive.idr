module Client.Action.Archive

import Data.Buffer
import Data.Maybe
import Extra.Buffer
import Inigo.Archive.Archive
import Inigo.Async.Archive
import Inigo.Async.Promise
import Inigo.Package.Package
import Inigo.Async.FS
import Inigo.Async.Package
import System.Path

export
buildArchive : String -> String -> Promise ()
buildArchive packageFile outFile =
  do
    package <- Inigo.Async.Package.readPackage packageFile
    Inigo.Async.Archive.saveArchive package (fromMaybe "" $ parent packageFile) outFile

export
extractArchive : String -> String -> Promise ()
extractArchive archiveFile outPath =
  do
    archive <- fs_readFileBuf archiveFile
    Inigo.Async.Archive.extractArchive archive outPath
