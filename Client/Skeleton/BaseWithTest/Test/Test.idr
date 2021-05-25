module Client.Skeleton.BaseWithTest.Test.Test

import Fmt

export
name : (String, String) -> List String
name (packageNS, packageName) = ["Test", packageName ++ "Test.idr"]

export
build : (String, String) -> String
build (packageNS, packageName) = fmt """
module Test.%sTest

import IdrTest.Test
import IdrTest.Expectation

import %s

export
suite : Test
suite =
  describe \"%s Tests\"
    [ test \"1 == 1\" (\\_ => assertEq 1 1 )
    ]
""" packageName packageName packageName
