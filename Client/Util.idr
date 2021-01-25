module Client.Util

import Data.Buffer
import Extra.Buffer
import Fmt
import Inigo.Async.Base
import Inigo.Async.Promise
import System
import System.File
import System.Path

rejectStatus : String -> Int -> Buffer -> Promise a
rejectStatus url status buf =
  do
    contents <- liftIO $ readAll buf
    let extra = if status == 500 then " -- You may need to login again with `inigo login`" else ""
    reject (fmt "HTTP Failure (%d) from %s: \"%s\"%s" status url contents extra)

export
assertOk : String -> Promise (Int, Buffer) -> Promise Buffer
assertOk url result =
  do
    (200, buf) <- result
      | (status, res) => rejectStatus url status res
    pure buf


||| TODO: Improve home dir
export
getHomeDir : IO (Maybe String)
getHomeDir =
  do
    Nothing <- getEnv "INIGO_HOME"
      | Just home => pure $ Just home
    Nothing <- getEnv "HOME"
      | Just home => pure $ Just home
    pure Nothing

export
inigoSessionFile : IO (Maybe String)
inigoSessionFile =
  do
    Nothing <- getEnv "INIGO_SESSION"
      | Just sessionFile => pure (Just sessionFile)
    Just homeDir <- getHomeDir
      | Nothing => pure Nothing
    pure $ Just (homeDir </> ".inigo_session")

export
readSessionFile : IO (Maybe String)
readSessionFile =
  do
    Just sessionFile <- inigoSessionFile
      | Nothing => pure Nothing
    Right session <- readFile sessionFile
      | Left err => pure Nothing
    pure (Just session)

export
auth : String -> (String, String)
auth session =
  ("Authorization", "Basic " ++ session)
