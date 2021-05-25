module Test.Inigo.Package.PackageTest

import IdrTest.Test
import IdrTest.Expectation

import Inigo.Async.Promise
import Inigo.Package.Package
import SemVar.Data

%default total

export
examplePackage : Package
examplePackage =
  MkPackage
    "Example"
    "Inigo"
    (MkVersion 0 0 1 (Just "pre") Nothing)
    (Just "My Description")
    (Just "http://example.com")
    (Just "README.md")
    ["MyDir"]
    ["idris2"]
    (Just "MIT")
    (Just "MyFile")
    (Just "MyExec")
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

examplePackageTest : Test
examplePackageTest =
  test "Parse Example Package" (\_ => assertEq
    (parsePackage """
      ns='Example'
      package='Inigo'
      version='0.0.1-pre'
      
      description='My Description'
      readme='README.md'
      link='http://example.com'
      modules=['MyDir']
      depends=['idris2']
      license='MIT'
      main='MyFile'
      executable='MyExec'
      
      [deps]
      MyDep='^1.2.3'
      
      [dev-deps]
      MyDevDep='2.0.0'
      """
    )
    (Right examplePackage)
  )

invalidVersionTest : Test
invalidVersionTest =
  test "Invalid version test" (\_ => assertEq
    (parsePackage
      """
      ns='Example'
      package='Inigo'
      version='0.0.1zz'
      """
    )
    (
      Left "Invalid version: 0.0.1zz"
    )
  )

-- Note: this is a little ugly, but it's technically correct
encodePackageTest : Test
encodePackageTest =
  test "Encode Example Package" (\_ => assertEq
    (encodePackage $
      MkPackage
        "Example"
        "Inigo"
        (MkVersion 0 0 1 (Just "pre") Nothing)
        (Just "My Description")
        (Just "http://example.com")
        (Just "README.md")
        ["MyDir"]
        ["idris2"]
        (Just "MIT")
        (Just "MyFile")
        (Just "MyExec")
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
    (
      """
      ns=\"Example\"
      package=\"Inigo\"
      version=\"0.0.1-pre\"
      description=\"My Description\"
      link=\"http://example.com\"
      readme=\"README.md\"
      modules=[\"MyDir\"]
      depends=[\"idris2\"]
      license=\"MIT\"
      main=\"MyFile\"
      executable=\"MyExec\"
      deps.MyDep=\">=1.2.3 <2.0.0\"
      dev-deps.MyDevDep=\"=2.0.0\"
      """
    )
  )

generateIPkgTest : Test
generateIPkgTest =
  test "Encode IPkg" (\_ => assertEq
    (generateIPkg
      True $ MkPackage
        "Example"
        "Inigo"
        (MkVersion 0 0 1 (Just "pre") Nothing)
        (Just "My Description")
        (Just "http://example.com")
        (Just "README.md")
        ["MyDir"]
        ["idris2"]
        (Just "MIT")
        (Just "MyFile")
        (Just "MyExec")
        [
          (["MyDep"], (
            AND
              (GTE $ MkVersion 1 2 3 Nothing Nothing)
              (LT $ MkVersion 2 0 0 Nothing Nothing)
          ))
        ]
        []
    )
    (
      """
      package Inigo

      modules = MyDir
      depends = idris2

      sourcedir = \"Deps/Example/Inigo\"

      version = \"0.0.1-pre\"
      main = MyFile
      executable = MyExec
      """
    )
  )

export
suite : Test
suite =
  describe "Package" [
    examplePackageTest,
    invalidVersionTest,
    encodePackageTest,
    generateIPkgTest
  ]
