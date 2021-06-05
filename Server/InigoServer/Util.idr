module Server.InigoServer.Util

import Data.Either
import Data.String
import Inigo.Async.Base
import Inigo.Async.CloudFlare.Worker
import Inigo.Async.Promise
import Inigo.Package.PackageIndex
import Server.Template.Template

export
renderLayout : String -> String -> String
renderLayout layout contents =
  if layout == "" then -- Allow empty or missing layout
    contents
  else
    template layout [("CONTENTS", contents)]

export
sniffContentType : String -> Headers
sniffContentType path =
  if isSuffixOf ".css" path
    then [("Content-Type", "text/css")]
    else if isSuffixOf ".svg" path
      then [("Content-Type", "image/svg+xml")]
      else []

export
withIndex : String -> String
withIndex "/" = "/home"
withIndex s = s

export
buildPkgLink : PackageMeta -> String
buildPkgLink (MkPackageMeta ns package description version) =
  let
    qualifiedName = ns ++ "." ++ package
    packagePath = "/packages/" ++ ns ++ "/" ++ package
    desc = case description of
      Just d =>
        " - " ++ d

      Nothing =>
        ""
  in
    " * [" ++ qualifiedName ++ "](" ++ packagePath ++ ")" ++ desc

||| Helpful default function for kv which returns empty strings
export
def : a -> (String -> a) -> String -> a
def d f "" = d
def d f str = f str

||| Raise an Either to a promise of its right prong
export
expectResult : Show err => Either err a -> Promise a
expectResult (Right x) = lift x
expectResult (Left err) = reject (show err)

||| Raise an Either to a promise of its right prong
export
expect : Show err => err -> Maybe a -> Promise a
expect err x = expectResult (maybeToEither err x)
