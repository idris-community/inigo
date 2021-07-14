module Inigo.Package.ExtraDep

import Data.List1
import Data.String
import Extra.String
import Inigo.Package.ParseHelpers
import Inigo.Paths
import System.Path
import Toml

-- TODO: Other methods?
public export
data Download
    = Git
    | SubDir

public export
DownloadInfo : Download -> Type
DownloadInfo Git = String -- commit
DownloadInfo SubDir = ()

public export
record ExtraDep where
    constructor MkExtraDep
    download : Download
    downloadInfo : DownloadInfo download
    url : String
    subDirs : List String

export
Show ExtraDep where
    show (MkExtraDep Git commit url subDirs) =
        "MkExtraDep{download=git, download-info=\"\{commit}\", url=\"\{url}\", subDirs=\{show subDirs}}"
    show (MkExtraDep SubDir () url subDirs) =
        "MkExtraDep{download=folder, url=\"\{url}\", subDirs=\{show subDirs}}"


export
Eq ExtraDep where
    MkExtraDep Git commit0 url0 subDirs0 == MkExtraDep Git commit1 url1 subDirs1 =
        commit0 == commit1 && url0 == url1 && subDirs0 == subDirs1
    MkExtraDep SubDir _ url0 subDirs0 == MkExtraDep SubDir _ url1 subDirs1 =
        url0 == url1 && subDirs0 == subDirs1
    _ == _ = False

export
eqIgnoreSubDirs : ExtraDep -> ExtraDep -> Bool
MkExtraDep Git commit0 url0 _ `eqIgnoreSubDirs` MkExtraDep Git commit1 url1 _ =
    commit0 == commit1 && url0 == url1
MkExtraDep SubDir _ url0 _ `eqIgnoreSubDirs` MkExtraDep SubDir _ url1 _ =
    url0 == url1
_ `eqIgnoreSubDirs` _ = False

export
toToml : List ExtraDep -> Toml
toToml deps = [(["extra-dep"], ArrTab $ depToToml <$> deps)]
  where
    depToToml : ExtraDep -> Toml
    depToToml (MkExtraDep Git commit url subDirs) =
        [ (["download"], Str "git")
        , (["commit"], Str commit)
        , (["url"], Str url)
        , (["sub-folders"], Lst (Str <$> subDirs))
        ]
    depToToml (MkExtraDep SubDir () url subDirs) =
        [ (["download"], Str "folder")
        , (["url"], Str url)
        , (["sub-folders"], Lst (Str <$> subDirs))
        ]

export
parseDownload : Toml -> Either String Download
parseDownload toml = case get ["download"] toml of
    Just (Str "git") => Right Git
    Just (Str "folder") => Right SubDir
    Just (Str val) => Left "Invalid download method: \{val}"
    Just val => Left "Inavlid value for field download: \{show val}"
    Nothing => Left "Missing field download"


export
parseExtraDeps : Toml -> Either String (List ExtraDep)
parseExtraDeps toml = case get ["extra-dep"] toml of
    Nothing => Right []
    Just (ArrTab deps) => foldlM extraDep [] deps
    Just val => Left "Invalid extra dependency found: \{show val}"
  where
    sanitiseCommit : String -> Either String String
    sanitiseCommit str =
        let len = length str
            allHex = all isHexDigit $ unpack str
        in if len == 7 || len == 40 && allHex
            then Right str
            else Left "Invalid commit: \"\{str}\""

    extraDep : List ExtraDep -> Toml -> Either String (List ExtraDep)
    extraDep deps toml = do
        url <- string ["url"] toml
        subDirs <- withDefault [""] $ listStr ["sub-folders"] toml
        download <- parseDownload toml
        case download of
            Git => do
                commit <- string ["commit"] toml >>= sanitiseCommit
                pure $ MkExtraDep Git commit url subDirs :: deps
            SubDir => do
                pure $ MkExtraDep SubDir () url subDirs :: deps

genFolder : String -> String
genFolder = concat . map escapeChar . unpack
  where
    escapeChar : Char -> String
    escapeChar c = if isAlphaNum c || c == '@'
        then cast c
        else "_" ++ show (ord c)

export
getExtraDepDir : ExtraDep -> String
getExtraDepDir (MkExtraDep Git commit url _) = inigoDepDir </> genFolder "\{url}@\{commit}"
getExtraDepDir (MkExtraDep SubDir _ url _) = url
