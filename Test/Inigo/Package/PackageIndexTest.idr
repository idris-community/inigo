module Test.Inigo.Package.PackageIndexTest

import IdrTest.Test
import IdrTest.Expectation

import Data.List
import Inigo.Async.Promise
import Inigo.Package.PackageIndex
import SemVar.Data
import Test.Inigo.Package.PackageTest
import Toml

exampleIndexToml : String
exampleIndexToml = """
[[pkg]]
ns=\"Example\"
package=\"Cool\"
description=\"cool package\"
version=\"1.2.3\"

[[pkg]]
ns=\"Example\"
package=\"Funk\"
description=\"funky package\"
version=\"10.0.0\"
"""

exampleIndex : PackageIndex
exampleIndex =
  [ MkPackageMeta "Example" "Cool" (Just "cool package") (MkVersion 1 2 3 Nothing Nothing)
  , MkPackageMeta "Example" "Funk" (Just "funky package") (MkVersion 10 0 0 Nothing Nothing)
  ]

exampleIndexExp : PackageIndex
exampleIndexExp =
  [ MkPackageMeta "Example" "Cool" (Just "cool package") (MkVersion 1 2 3 Nothing Nothing)
  , MkPackageMeta "Example" "Inigo" (Just "what pkg") (MkVersion 0 10 0 Nothing Nothing)
  , MkPackageMeta "Example" "Funk" (Just "funky package") (MkVersion 10 0 0 Nothing Nothing)
  ]

exampleInigoMeta : PackageMeta
exampleInigoMeta =
  MkPackageMeta "Example" "Inigo" (Just "My Description") (MkVersion 0 0 1 (Just "pre") Nothing)

toTomlTest : Test
toTomlTest =
  test "To Toml" (\_ => assertEq
    (toToml exampleIndex)
    [(["pkg"],
      ArrTab
        [
          [ (["ns"], Str "Example")
          , (["package"], Str "Cool")
          , (["description"], Str "cool package")
          , (["version"], Str "1.2.3")
          ],
          [ (["ns"], Str "Example")
          , (["package"], Str "Funk")
          , (["description"], Str "funky package")
          , (["version"], Str "10.0.0")
          ]
        ]
    )]
  )

encodePackageIndexTest : Test
encodePackageIndexTest =
  test "Encode package index" (\_ => assertEq
    (encodePackageIndex exampleIndex)
    exampleIndexToml
  )

toPackageMetaTest : Test
toPackageMetaTest =
  test "fromPackage" (\_ => assertEq
    (fromPackage examplePackage)
    exampleInigoMeta
  )

updatePackageMetaNewTest : Test
updatePackageMetaNewTest =
  test "updatePackageMeta New Package" (\_ => assertEq
    (
      updatePackageMeta
        exampleIndex
        examplePackage
    )
    (snoc exampleIndex exampleInigoMeta)
  )

updatePackageMetaExistingTest : Test
updatePackageMetaExistingTest =
  test "updatePackageMeta Existing Package" (\_ => assertEq
    (
      updatePackageMeta
        exampleIndexExp
        examplePackage
    )
    [ MkPackageMeta "Example" "Cool" (Just "cool package") (MkVersion 1 2 3 Nothing Nothing)
    , MkPackageMeta "Example" "Inigo" (Just "My Description") (MkVersion 0 0 1 (Just "pre") Nothing)
    , MkPackageMeta "Example" "Funk" (Just "funky package") (MkVersion 10 0 0 Nothing Nothing)
    ]
  )

export
suite : Test
suite =
  describe "Package Index"
    [ test "Parse Example Index" (\_ => assertEq
        (parsePackageIndex exampleIndexToml)
        (Right exampleIndex)
      )
    , test "Search Index" (\_ => assertEq
        (((map $ searchPackageIndex "fun") . parsePackageIndex) exampleIndexToml)
        (Right $ 
          [ MkPackageMeta "Example" "Funk" (Just "funky package") (MkVersion 10 0 0 Nothing Nothing)
          ]
        )
      )
    , toTomlTest
    , encodePackageIndexTest
    , toPackageMetaTest
    , updatePackageMetaNewTest
    , updatePackageMetaExistingTest
    ]
