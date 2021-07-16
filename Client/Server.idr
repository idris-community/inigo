module Client.Server

import Inigo.Package.Package
import Extra.String
import SemVar

public export
data Server : Type where
  Dev : Server
  Prod : Server

export
toString : Server -> String
toString server =
  case server of
    Dev =>
      "dev"
    Prod =>
      "prod"

export
getServer : String -> Maybe Server
getServer str =
  case str of
    "dev" =>
      Just Dev
    "prod" =>
      Just Prod
    _ =>
      Nothing

export
host : Server -> String
host Dev = "http://localhost:3000/"
host Prod = "https://inigo.pm/"

export
getArchiveUrl : Package -> String
getArchiveUrl pkg =
  join "/"
    [ "api"
    , "archive"
    , ns pkg
    , package pkg
    , (show . version) pkg
    ]

export
postArchiveUrl : Package -> String
postArchiveUrl pkg =
  join "/"
    [ "api"
    , "archive"
    , ns pkg
    , package pkg
    , (show . version) pkg
    , "post"
    ]

export
postReadmeUrl : Package -> String
postReadmeUrl pkg =
  join "/"
    [ "api"
    , "readme"
    , ns pkg
    , package pkg
    , (show . version) pkg
    , "post"
    ]

export
postPackageUrl : Package -> String
postPackageUrl pkg =
  join "/"
    [ "api"
    , "packages"
    , ns pkg
    , package pkg
    , (show . version) pkg
    , "post"
    ]

export
getPackageUrl : String -> String -> String
getPackageUrl packageNS packageName =
  join "/"
    [ "api"
    , "packages"
    , packageNS
    , packageName
    , "conf"
    ]

export
getPackageVersionUrl : String -> String -> Version -> String
getPackageVersionUrl packageNS packageName version =
  join "/"
    [ "api"
    , "packages"
    , packageNS
    , packageName
    , show version
    , "conf"
    ]

export
getPackageDepTreeUrl : String -> String -> String
getPackageDepTreeUrl packageNS packageName =
  join "/"
    [ "api"
    , "packages"
    , packageNS
    , packageName
    , "deps"
    ]

export
accountPostUrl : String -> String
accountPostUrl ns =
  join "/"
    [ "api"
    , "account"
    , ns
    , "post"
    ]

export
accountLoginUrl : String -> String
accountLoginUrl ns =
  join "/"
    [ "api"
    , "account"
    , ns
    , "login"
    ]
