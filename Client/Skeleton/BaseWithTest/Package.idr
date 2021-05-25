module Client.Skeleton.BaseWithTest.Package

import Fmt

export
name : (String, String) -> List String
name (packageNS, packageName) = [packageName ++ ".idr"]

export
build : (String, String) -> String
build (packageNS, packageName) = fmt """
module %s

main : IO ()
main =
  putStrLn \"Hello from %s.%s\"
""" packageName packageNS packageName
