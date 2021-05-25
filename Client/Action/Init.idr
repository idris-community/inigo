module Client.Action.Init

import Client.Skeleton.Skeleton
import Data.List
import Fmt
import Inigo.Async.Base
import Inigo.Async.FS
import Inigo.Async.Promise
import System.Path

export
init : Skeleton -> String -> String -> Promise ()
init skeleton packageNS packageName =
  do
    ignore $ all $ map writeTmplFile (getFiles skeleton (packageNS, packageName))
    log (fmt "Successfully built %s" (toString skeleton))
  where
    ensureParent : String -> Promise ()
    ensureParent path = case parent path of
                          Just parentPath => fs_mkdir True parentPath
                          Nothing => pure ()

    writeTmplFile : (List String, String) -> Promise ()
    writeTmplFile (path, contents) =
      do
        let path = joinPath path
        ensureParent path
        fs_writeFile path contents
