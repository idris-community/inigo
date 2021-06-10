module Inigo.Async.Git

import Inigo.Async.Base
import Inigo.Async.Promise
import Inigo.Async.FS
import System.Path

||| Download a git repository optionally specifying the commit, then remove the .git folder
||| Returns `True` iff the file already exists.
export
git_downloadTo : (url : String) -> (commit : Maybe String) -> (dest : String) -> Promise Bool
git_downloadTo url commit dest = do
    case !(fs_exists dest) of
        False => do
            ignore $ system "git" ["clone", "-q", "--progress", "--recurse-submodules", url, dest] Nothing False False
            maybe
                (pure ())
                (\com => ignore $ system "git" ["checkout", "-q", com] (Just dest) False False)
                commit
            fs_rmdir True (dest </> ".git")
            pure False
        True => pure True
