module Inigo.Archive.Path

import Data.List
import Data.String
import Extra.String
import Inigo.Package.Package
import System.Path

isIPkg : String -> Bool
isIPkg =
  isSuffixOf ".ipkg"

isBuild : String -> Bool
isBuild =
  isInfixOf "/build/"

isDep : String -> Bool
isDep =
  isInfixOf "/Deps/"

isDisallowed : String -> Bool
isDisallowed x =
  isIPkg x || isBuild x || isDep x

export
ignoreFiles : List String -> List String
ignoreFiles =
  filter (not . isDisallowed)

export
depPath : Package -> String
depPath pkg =
  let
    modPath = joinPath (split '.' (package pkg))
  in
    "Deps" </> (ns pkg) </> modPath
