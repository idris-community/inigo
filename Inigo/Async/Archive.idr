module Inigo.Async.Archive

import Data.Buffer
import Data.List
import Data.Maybe
import Data.Strings
import Extra.Buffer
import Extra.String
import Fmt
import Inigo.Archive.Archive as Archive
import Inigo.Archive.Path
import Inigo.Async.Base
import Inigo.Async.Compress.Brotli
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Package.Package
import System
import System.Path

-- Simple archive format for storing files compressed in Toml files

-- Overall, this file is still a bit complex and does
-- too much in promises, but it's a start

compressFileEnc64 : String -> Promise String
compressFileEnc64 path =
  do
    compressed <- compressFile path
    liftIO $ enc64 compressed

export
buildArchive : Package -> String -> Promise String
buildArchive package rootPath =
  do
    allFiles <- fs_getFilesR rootPath
    -- We're going to skip `ipkg` files
    let files = ignoreFiles allFiles
    enc <- all (map encodeFile files)
    pure (Archive.encode enc)
  where
    encodeFile : String -> Promise (List String, String)
    encodeFile file =
      do
        res <- compressFileEnc64 file
        let relPath = fromMaybe "" $ Path.dropBase rootPath file
        log (fmt "Archiving %s..." relPath)
        pure (splitPath relPath, res)

export
saveArchive : Package -> String -> String -> Promise ()
saveArchive package rootPath outFile =
  do
    contents <- buildArchive package rootPath
    fs_writeFile outFile contents
    log ("Wrote archive " ++ outFile)

decompressFile : String -> (List String, String) -> Promise ()
decompressFile outPath (filename, compressed) =
  do
    let resultFile = outPath </> (joinPath filename)
    buffer <- liftIO $ dec64 compressed
    res <- decompress buffer
    log ("Decompressing " ++ resultFile ++ "...")
    pure ()
    let resultDir = fromMaybe "" $ parent resultFile
    fs_mkdir True resultDir
    fs_writeFileBuf resultFile res

expect : String -> Maybe a -> Promise a
expect msg (Just val) = pure val
expect msg Nothing = reject msg

export
extractArchive : Buffer -> String -> Promise ()
extractArchive archive outPath =
  do
    contents <- liftIO (readAll archive)
    files <- expect "Failed to read archive" (Archive.decode contents)
    ignore $ all (map (decompressFile outPath) files)
