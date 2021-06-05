module SemVar.Data

import Data.String
import Data.List
import Data.Maybe

import Extra.String

%default total

public export
record Version where
  constructor MkVersion
  major : Int
  minor : Int
  patch : Int
  release : Maybe String
  metadata : Maybe String

%name Version version

export
Show Version where
  show version =
    join "" [ show (major version)
    , "."
    , show (minor version)
    , "."
    , show (patch version)
    , ((fromMaybe "") . map ((++) "-")) (release version)
    , ((fromMaybe "") . map ((++) "+")) (metadata version)
    ]

export
Eq Version where
  version0 == version1 =
    (major version0) == (major version1) &&
    (minor version0) == (minor version1) &&
    (patch version0) == (patch version1) &&
    (release version0) == (release version1) &&
    (metadata version0) == (metadata version1)

public export
data Requirement : Type where
  GT : Version -> Requirement
  GTE : Version -> Requirement
  LT : Version -> Requirement
  LTE : Version -> Requirement
  EQ : Version -> Requirement
  AND : Requirement -> Requirement -> Requirement
  OR : Requirement -> Requirement -> Requirement

%name Requirement requirement

-- TODO: Improve show with a pretty form, if possible
export
Show Requirement where
  show (AND req0 req1) = (show req0) ++ " " ++ (show req1)
  show (OR req0 req1) = (show req0) ++ " || " ++ (show req1)
  show (GT version) = ">" ++ (show version)
  show (GTE version) = ">=" ++ (show version)
  show (LT version) = "<" ++ (show version)
  show (LTE version) = "<=" ++ (show version)
  show (EQ version) = "=" ++ (show version)

export
Eq Requirement where
  (AND req0a req0b) == (AND req1a req1b) = req0a == req1a && req0b == req1b
  (OR req0a req0b) == (OR req1a req1b) = req0a == req1a && req0b == req1b
  (GT version0) == (GT version1) = version0 == version1
  (GTE version0) == (GTE version1) = version0 == version1
  (LT version0) == (LT version1) = version0 == version1
  (LTE version0) == (LTE version1) = version0 == version1
  (EQ version0) == (EQ version1) = version0 == version1
  _ == _ = False

export
nextMinor : Version -> Version
nextMinor version =
  MkVersion (major version) ((minor version) + 1) 0 Nothing Nothing

export
nextMajor : Version -> Version
nextMajor (MkVersion 0 minor _ _ _) =
  MkVersion 0 (minor + 1) 0 Nothing Nothing
nextMajor version =
  MkVersion ((major version) + 1) 0 0 Nothing Nothing

export
Ord Version where
  compare (MkVersion major0 minor0 patch0 release0 metadata0) (MkVersion major1 minor1 patch1 release1 metadata1) =
      case compare major0 major1 of
        EQ =>
          case compare minor0 minor1 of
            EQ =>
              case compare patch0 patch1 of
                EQ =>
                  case (release0, release1) of
                    (Just _, Nothing) =>
                      LT
                    (Nothing, Just _) =>
                      GT
                    _ =>
                      EQ
                els =>
                  els
            els =>
              els
        els =>
          els

gte : Version -> Version -> Bool
gte v0 v1 =
  let cmp = compare v0 v1
  in cmp == GT || cmp == EQ

lte : Version -> Version -> Bool
lte v0 v1 =
  let cmp = compare v0 v1
  in cmp == LT || cmp == EQ

-- TODO: We might want to make this dependently typed
export
satisfy : Requirement -> Version -> Bool
satisfy (AND reqA reqB) version = (satisfy reqA version) && (satisfy reqB version)
satisfy (OR reqA reqB) version = (satisfy reqA version) || (satisfy reqB version)
satisfy (GTE v0) v1 = v1 `gte` v0
satisfy (GT v0) v1 = compare v1 v0 == GT
satisfy (LTE v0) v1 = v1 `lte` v0
satisfy (LT v0) v1 = compare v1 v0 == LT
satisfy (EQ v0) v1 = compare v1 v0 == EQ
satisfy _ _ = False
