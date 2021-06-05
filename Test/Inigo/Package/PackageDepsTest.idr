module Test.Inigo.Package.PackageDepsTest

import IdrTest.Test
import IdrTest.Expectation

import Data.List
import Inigo.Async.Promise
import Inigo.Package.PackageDeps
import SemVar.Data
import Test.Inigo.Package.PackageTest
import Toml

exampleDepsToml : String
exampleDepsToml = """
['1.2.3']
deps.a='=5.0.0'
dev.b='=6.0.0'

['1.2.4']
deps.a='=5.1.0'
dev.b='=6.1.0'
"""

exampleDepsExp : PackageDeps
exampleDepsExp =
  [ ((MkVersion 1 2 3 Nothing Nothing), (
      MkPackageDep
        [(["a"], (EQ (MkVersion 5 0 0 Nothing Nothing)))]
        [(["b"], (EQ (MkVersion 6 0 0 Nothing Nothing)))]
    ))
  , ((MkVersion 1 2 4 Nothing Nothing), (
      MkPackageDep
        [(["a"], (EQ (MkVersion 5 1 0 Nothing Nothing)))]
        [(["b"], (EQ (MkVersion 6 1 0 Nothing Nothing)))]
    ))
  ]

exampleDepsExpExt : PackageDeps
exampleDepsExpExt =
  [ ((MkVersion 1 2 3 Nothing Nothing), (
      MkPackageDep [] []
    ))
  , ((MkVersion 0 0 1 (Just "pre") Nothing), (
      MkPackageDep [] []
    ))
  , ((MkVersion 1 2 4 Nothing Nothing), (
      MkPackageDep [] []
    ))
  ]

toTomlTest : Test
toTomlTest =
  test "To Toml" (\_ => assertEq
    (toToml exampleDepsExp)
    [ ( ["1.2.3", "deps", "a"], Str "=5.0.0" )
    , ( ["1.2.3", "dev", "b"], Str "=6.0.0" )
    , ( ["1.2.4", "deps", "a"], Str "=5.1.0" )
    , ( ["1.2.4", "dev", "b"], Str "=6.1.0" )
    ]
  )

encodePackageDepsTest : Test
encodePackageDepsTest =
  test "Encode package index" (\_ => assertEq
    (encodePackageDeps exampleDepsExp)
    "\"1.2.3\".deps.a=\"=5.0.0\"\n\"1.2.3\".dev.b=\"=6.0.0\"\n\"1.2.4\".deps.a=\"=5.1.0\"\n\"1.2.4\".dev.b=\"=6.1.0\""
  )

toPackageDepTest : Test
toPackageDepTest =
  test "fromPackage" (\_ => assertEq
    (fromPackage examplePackage)
    (MkPackageDep
      [
        (["MyDep"], (
          AND
            (GTE $ MkVersion 1 2 3 Nothing Nothing)
            (LT $ MkVersion 2 0 0 Nothing Nothing)
        ))
      ]
      [
        (["MyDevDep"], (
          EQ (MkVersion 2 0 0 Nothing Nothing)
        ))
      ]
    )
  )

exampleDep : (Version, PackageDep)
exampleDep =
  (
    (MkVersion 0 0 1 (Just "pre") Nothing),
    (
      MkPackageDep
        [
          (["MyDep"], (
            AND
              (GTE $ MkVersion 1 2 3 Nothing Nothing)
              (LT $ MkVersion 2 0 0 Nothing Nothing)
          ))
        ]
        [
          (["MyDevDep"], (
            EQ (MkVersion 2 0 0 Nothing Nothing)
          ))
        ]
    )
  )

updatePackageDepNewTest : Test
updatePackageDepNewTest =
  test "updatePackageDep New Dep" (\_ => assertEq
    (
      updatePackageDep
        exampleDepsExp
        examplePackage
    )
    (snoc exampleDepsExp exampleDep)
  )

updatePackageDepExistingTest : Test
updatePackageDepExistingTest =
  test "updatePackageDep Existing Dep" (\_ => assertEq
    (
      updatePackageDep
        exampleDepsExpExt
        examplePackage
    )
    [ ((MkVersion 1 2 3 Nothing Nothing), (
        MkPackageDep [] []
      ))
    , ((MkVersion 0 0 1 (Just "pre") Nothing), (
        MkPackageDep
        [
          (["MyDep"], (
            AND
              (GTE $ MkVersion 1 2 3 Nothing Nothing)
              (LT $ MkVersion 2 0 0 Nothing Nothing)
          ))
        ]
        [
          (["MyDevDep"], (
            EQ (MkVersion 2 0 0 Nothing Nothing)
          ))
        ]
      ))
    , ((MkVersion 1 2 4 Nothing Nothing), (
        MkPackageDep [] []
      ))
    ]
  )

export
suite : Test
suite =
  describe "PackageDeps"
    [ test "Parse Deps" (\_ => assertEq
        (parsePackageDeps exampleDepsToml)
        (Right exampleDepsExp)
      )
    , toTomlTest
    , encodePackageDepsTest
    , toPackageDepTest
    , updatePackageDepNewTest
    , updatePackageDepExistingTest
    ]
