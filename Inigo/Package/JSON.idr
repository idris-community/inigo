||| Module for serialising and deserialising Inigo packages
module Inigo.Package.JSON

import Data.List
import Data.Maybe
import Inigo.Package.Package
import Language.JSON
import SemVar

extraDepToJSON : ExtraDep -> JSON
extraDepToJSON (MkExtraDep Git commit url subDirs) = JObject
    [ ("download", JString "git")
    , ("commit", JString commit)
    , ("url", JString url)
    , ("subDirs", JArray (JString <$> subDirs))
    ]

nullable : Lazy (a -> JSON) -> Maybe a -> JSON
nullable = maybe JNull

export
encodePackage : Package -> JSON
encodePackage pkg = JObject
    [ ("ns", JString pkg.ns)
    , ("package", JString pkg.package)
    , ("version", JString (show pkg.version))
    , ("description", nullable JString pkg.description)
    , ("link", nullable JString pkg.link)
    , ("readme", nullable JString pkg.readme)
    , ("modules", JArray (JString <$> pkg.modules))
    , ("depends", JArray (JString <$> pkg.depends))
    , ("license", nullable JString pkg.license)
    , ("sourcedir", JString pkg.sourcedir)
    , ("main", nullable JString pkg.main)
    , ("executable", nullable JString pkg.executable)
    , ("deps", JArray (mkDep <$> pkg.deps))
    , ("dev-deps", JArray (mkDep <$> pkg.devDeps))
    , ("extra-deps", JArray (extraDepToJSON <$> pkg.extraDeps))
    ]
  where
    mkDep : (List String, Requirement) -> JSON
    mkDep (ns, semvar) = JObject
        [ ("name", JArray (JString <$> ns))
        , ("semvar", JString (show semvar))
        ]

export
encodeAsJSON : Package -> String
encodeAsJSON = show . JSON.encodePackage

namespace Parse

    key : String -> JSON -> Maybe JSON
    key k (JObject kvs) = lookup k kvs
    key _ _ = Nothing

    string : JSON -> Maybe String
    string (JString str) = Just str
    string _ = Nothing

    stringKey : String -> JSON -> Maybe String
    stringKey k json = key k json >>= \case
        JString str => Just str
        _ => Nothing

    arrayKey : String -> JSON -> Maybe (List JSON)
    arrayKey k json = key k json >>= \case
        JArray arr => Just arr
        _ => Nothing

    nullable : Maybe a -> Maybe (Maybe a)
    nullable (Just x) = Just (Just x)
    nullable Nothing = Just Nothing

    parseExtraDep : JSON -> Maybe ExtraDep
    parseExtraDep json = do
        url <- stringKey "url" json
        subDirs <- arrayKey "subDirs" json >>= traverse string
        download <- stringKey "download" json
        case download of
            "git" => do
                commit <- stringKey "commit" json
                Just $ MkExtraDep Git commit url subDirs
            _ => Nothing

    export
    parsePackage : JSON -> Maybe Package
    parsePackage json = do
        ns <- stringKey "ns" json
        package <- stringKey "package" json
        version <- stringKey "version" json >>= parseVersion
        description <- nullable $ stringKey "description" json
        link <- nullable $ stringKey "link" json
        readme <- nullable $ stringKey "readme" json
        modules <- arrayKey "modules" json >>= traverse string
        depends <- arrayKey "depends" json >>= traverse string
        license <- nullable $ stringKey "license" json
        sourcedir <- stringKey "sourcedir" json
        main <- nullable $ stringKey "main" json
        executable <- nullable $ stringKey "executable" json
        deps <- arrayKey "deps" json >>= traverse parseDep
        devDeps <- arrayKey "dev-deps" json >>= traverse parseDep
        extraDeps <- arrayKey "extra-deps" json >>= traverse parseExtraDep
        Just $ MkPackage
            { ns, package, version, description, link, readme
            , modules, depends, license, sourcedir
            , main, executable, deps, devDeps, extraDeps
            }
      where
        parseDep : JSON -> Maybe (List String, Requirement)
        parseDep json = do
            ns <- arrayKey "name" json >>= traverse string
            var <- stringKey "semvar" json >>= parseRequirement
            Just (ns, var)

export
decodeJSON : String -> Maybe Package
decodeJSON = parse >=> parsePackage
