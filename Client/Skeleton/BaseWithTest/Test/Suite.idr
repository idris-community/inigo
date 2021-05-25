module Client.Skeleton.BaseWithTest.Test.Suite

import Fmt

export
name : (String, String) -> List String
name = const ["Test", "Suite.idr"]

export
build : (String, String) -> String
build (packageNS, packageName) = fmt """
module Test.Suite

import IdrTest.Test

import Test.%sTest

suite : IO ()
suite = do
  runSuites
    [ Test.%sTest.suite
    ]
""" packageName packageName
