module Inigo.Package.Package

import Data.List
import Data.Maybe
import Extra.Either
import Extra.List
import Extra.String
import Fmt
import public Inigo.Package.ExtraDep
import Inigo.Package.ParseHelpers
import Inigo.Paths
import Inigo.PkgTree
import SemVar
import System.Path
import Toml

public export
record Package where
  constructor MkPackage
  ns : String
  package : String
  version : Version
  description : Maybe String
  link : Maybe String
  readme : Maybe String
  modules : List String
  depends : List String
  license : Maybe String
  sourcedir : String
  main : Maybe String
  executable : Maybe String
  deps : List (List String, Requirement)
  devDeps : List (List String, Requirement)
  extraDeps : List ExtraDep

public export
Show Package where
  show pkg =
    "Package{" ++
      (join "" [
        "ns=", (show $ ns pkg),
        ", package=", (show $ package pkg),
        ", version=", (show $ version pkg),
        ", description=", (show $ description pkg),
        ", link=", (show $ link pkg),
        ", readme=", (limit 50 $ show $ readme pkg),
        ", modules=", (show $ modules pkg),
        ", depends=", (show $ depends pkg),
        ", license=", (show $ license pkg),
        ", sourcedir=", (show $ sourcedir pkg),
        ", main=", (show $ main pkg),
        ", executable=", (show $ executable pkg),
        ", deps=", (show $ deps pkg),
        ", dev-deps=", (show $ devDeps pkg),
        ", extra-deps=", (show $ extraDeps pkg)
      ]) ++ "}"

public export
Eq Package where
  (MkPackage ns0 package0 version0 description0 link0 readme0 modules0 depends0 license0 src0 main0 executable0 deps0 devDeps0 extraDeps0)
    == (MkPackage ns1 package1 version1 description1 link1 readme1 modules1 depends1 license1 src1 main1 executable1 deps1 devDeps1 extraDeps1) =
      ns0 == ns1 && package0 == package1 && version0 == version1 && description0 == description1 && link0 == link1 && readme0 == readme1 &&
      modules0 == modules1 && depends0 == depends1 && license0 == license1 && src0 == src1 && main0 == main1 && executable0 == executable1 &&
      deps0 == deps1 && devDeps0 == devDeps1 && extraDeps0 == extraDeps1

export
parseDeps : List String -> Toml -> Either String (List (List String, Requirement))
parseDeps key toml =
  foldl parseDepsAcc (Right []) (getToml key toml)
  where
    parseDepsAcc : Either String (List (List String, Requirement)) -> (List String, Value) -> Either String (List (List String, Requirement))
    parseDepsAcc accE (key, el) =
      case el of
        Str val =>
          do
            acc <- accE
            dep <- expect ("Invalid requirement: " ++ show key ++ "=" ++ show val) (parseRequirement val)
            pure $ (key, dep) :: acc
        _ =>
          Left ("Invalid dep found: " ++ show key)

||| Attempts to parse a package
export
parsePackage : String -> Either String Package
parsePackage pkgToml =
  do
    toml <- expect "failed to parse Toml" $ parseToml pkgToml
    ns <- string ["ns"] toml
    package <- string ["package"] toml
    versionStr <- string ["version"] toml
    version <- expect ("Invalid version: " ++ versionStr) (parseVersion versionStr)
    description <- maybe (string ["description"] toml)
    link <- maybe (string ["link"] toml)
    readme <- maybe (string ["readme"] toml)
    modules <- listStr ["modules"] toml
    depends <- listStr ["depends"] toml
    license <- maybe (string ["license"] toml)
    sourcedir <- withDefault "" (string ["sourcedir"] toml)
    main <- maybe (string ["main"] toml)
    executable <- maybe (string ["executable"] toml)
    deps <- parseDeps ["deps"] toml
    devDeps <- parseDeps ["dev-deps"] toml
    extraDeps <- parseExtraDeps toml
    pure (MkPackage ns package version description link readme modules depends license sourcedir main executable deps devDeps extraDeps)

-- TODO: Get deps better, e.g. pin instead of `&&`
export
toToml : Package -> Toml
toToml pkg =
  (mapMaybe id [
    Just (["ns"], Str (ns pkg)),
    Just (["package"], Str (package pkg)),
    Just (["version"], Str (show (version pkg))),
    map (\desc => (["description"], Str desc)) (description pkg),
    map (\link => (["link"], Str link)) (link pkg),
    map (\desc => (["readme"], Str desc)) (readme pkg),
    Just (["modules"], Lst (map Str (modules pkg))),
    Just (["depends"], Lst (map Str (depends pkg))),
    map (\license => (["license"], Str license)) (license pkg),
    Just (["sourcedir"], Str (sourcedir pkg)),
    map (\main => (["main"], Str main)) (main pkg),
    map (\executable => (["executable"], Str executable)) (executable pkg)
  ]) ++ (depsVal "deps" (deps pkg)) ++ (depsVal "dev-deps" (devDeps pkg))
  ++ toToml pkg.extraDeps
  where
    depsVal : String -> List (List String, Requirement) -> List (List String, Value)
    depsVal baseKey = map (\(key, req) => (baseKey :: key, Str (show req)))

||| Encode a package, for instance, after adding a new dep
export
encodePackage : Package -> String
encodePackage = encode . toToml

whenCons : Monoid a => List b -> a -> a
whenCons [] _ = neutral
whenCons _ x = x

-- ||| Generates an ipkg for compatibility with the native idris build system
export
generateIPkg : (includeExe : Bool) -> Maybe String -> Package -> String
generateIPkg exe depBuildDir pkg =
  let
    main = fromMaybe "" $ map ("\nmain = " ++) (main pkg)
    executable = if exe
        then fromMaybe "" $ map ("\nexecutable = " ++) (executable pkg)
        else ""
    modules' = whenCons pkg.modules $ fmt "modules = %s " $ join ", " (modules pkg)
    depends' = whenCons pkg.depends $ fmt "depends = %s " $ join ", " (depends pkg)
    buildDir = fromMaybe "build" depBuildDir
  in
    fmt """
package %s

%s
%s

sourcedir = %s
builddir = %s

version = %s%s%s
""" (package pkg) modules' depends' (quote pkg.sourcedir) (quote buildDir) (show $ version pkg) main executable

export
getDepsPackage : Bool -> Package -> List String
getDepsPackage dev pkg =
    let deps : List (List String, Requirement)
        deps = (if dev then pkg.devDeps else []) ++ pkg.deps
    in depsOnto pkg.extraDeps $ map (joinPath . Builtin.fst) deps
    where
    depsOnto : List ExtraDep -> List String -> List String
    depsOnto [] deps = deps
    depsOnto (ed :: eds) deps =
        let dir = getExtraDepDir ed
        in depsOnto eds $ map (dir </>) ed.subDirs ++ deps

export
HasDeps Package String where
    getDeps = getDepsPackage
