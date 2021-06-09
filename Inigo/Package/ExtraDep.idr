module Inigo.Package.ExtraDep

import Data.List1
import Data.String
import Extra.String
import Inigo.Package.ParseHelpers
import Toml

-- TODO: Other methods?
public export
data Download
    = Git

public export
DownloadInfo : Download -> Type
DownloadInfo Git = String -- commit

export
parseDownload : Toml -> Either String Download
parseDownload toml = case get ["download"] toml of
    Just (Str "git") => Right Git
    Just (Str val) => Left "Invalid download method: \{val}"
    Just val => Left "Inavlid value for field download: \{show val}"
    Nothing => Left "Missing field download"

public export
record ExtraDep where
    constructor MkExtraDep
    download : Download
    downloadInfo : DownloadInfo download
    url : String
    subFolders : List String

export
Show ExtraDep where
    show (MkExtraDep Git commit url subFolders) =
        "MkExtraDep{download=git, download-info=\"\{commit}\", url=\"\{url}\", subFolders=\{show subFolders}"

export
Eq ExtraDep where
    MkExtraDep Git commit0 url0 subFolders0 == MkExtraDep Git commit1 url1 subFolders1 =
        commit0 == commit1 && url0 == url1 && subFolders0 == subFolders1
    _ == _ = False

export
toToml : List ExtraDep -> Toml
toToml deps = [(["extra-dep"], ArrTab $ depToToml <$> deps)]
  where
    depToToml : ExtraDep -> Toml
    depToToml (MkExtraDep Git commit url subFolders) =
        [ (["download"], Str "git")
        , (["commit"], Str commit)
        , (["url"], Str url)
        , (["sub-folders"], Lst (Str <$> subFolders))
        ]

export
parseExtraDeps : Toml -> Either String (List ExtraDep)
parseExtraDeps toml = case get ["extra-dep"] toml of
    Nothing => Right []
    Just (ArrTab deps) => foldlM extraDep [] deps
    Just val => Left "Invalid extra dependency found: \{show val}"
  where
    extraDep : List ExtraDep -> Toml -> Either String (List ExtraDep)
    extraDep deps toml = do
        url <- string ["url"] toml
        subFolders <- withDefault [""] $ listStr ["sub-folders"] toml
        download <- parseDownload toml
        case download of
            Git => do
                commit <- string ["commit"] toml
                pure $ MkExtraDep Git commit url subFolders :: deps

export
genFolder : String -> String
genFolder = concat . map escapeChar . unpack
  where
    escapeChar : Char -> String
    escapeChar c = if isAlphaNum c
        then cast c
        else "_" ++ show (ord c)
