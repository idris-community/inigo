module Server.InigoServer.Handler

import Data.Buffer
import Data.Either
import Data.List
import Data.Maybe
import Data.String
import Extra.Buffer
import Extra.Either
import Extra.String
import Fmt
import Inigo.Account.Account
import Inigo.Async.Base
import Inigo.Async.CloudFlare.KV as KV
import Inigo.Async.CloudFlare.Worker
import Inigo.Async.Package
import Inigo.Async.Promise
import Inigo.Async.SubtleCrypto.SubtleCrypto
import Inigo.Package.Package
import Inigo.Package.PackageDeps
import Inigo.Package.PackageIndex
import Markdown
import Markdown.Format.Html
import SemVar
import Server.InigoServer.KVOps as KVOps
import Server.InigoServer.Util as Util
import Server.Template.Template as Template
import Toml

limit : Nat
limit = 50

defaultHash : String -> Algorithm
defaultHash salt =
  PBKDF2 Sha256 salt 10000

-- TODO: While this is practical, we should
--       switch the server to use dependent types (e.g. App)
--       in order to clean up the mess of dependencies here
--       Note: this may be hard to do with Promises, but maybe
--       this can assist us in writing cleaner code!

-- TODO: Complete Request Data type
-- TODO: mapError
-- TODO: Parallel requests
handlePage : String -> List (String, String) -> Promise Response
handlePage path vars =
  do
    contents <- KV.read "pages" (Util.withIndex path)
    layout <- KV.read "static" "/layout.html" -- TODO: Request in paralel
    if contents == ""
      then pure (404, "Missing data for page", [])
      else do
        let tmplContents = Template.template contents vars
        case map toHtml $ Markdown.parse tmplContents of
          Nothing =>
            pure (500, "Failed to parse markdown", [])
          Just parsed =>
            pure (html (Util.renderLayout layout parsed))

handleApiArchive : String -> String -> Version -> Promise Response
handleApiArchive packageNS packageName version =
  do
    Just contents <- KVOps.readArchive packageNS packageName version
      | Nothing => pure (404, "not found", [])
    pure (200, contents, [])

handlePackageIndex : Promise Response
handlePackageIndex =
  do
    index <- KVOps.readIndex
    let packageList = map Util.buildPkgLink (take limit index)
    let vars = [("PACKAGES", join "\n" packageList)]
    handlePage "/packageIndex" vars

enumerate : String -> String -> Nat -> String
enumerate sing _ (S Z) = "1 " ++ sing
enumerate _ plur n = fmt "%d %s" (cast n) plur

handleSearch : String -> Promise Response
handleSearch term =
  do
    index <- KVOps.readIndex
    let packages = searchPackageIndex term index
    let packageList = map Util.buildPkgLink packages
    let vars =
      [ ("COUNT", enumerate "package" "packages" (length packageList))
      , ("TERM", term)
      , ("PACKAGES", join "\n" packageList)
      ]
    handlePage "/search" vars

handleApiPackageIndex : Promise Response
handleApiPackageIndex =
  do
    index <- KVOps.readIndex
    pure (200, encodePackageIndex index, [])

orLatestVersion : Maybe Version -> String -> String -> Promise (Maybe Version)
orLatestVersion (Just v) = const $ const $ lift (Just v)
orLatestVersion Nothing = latestVersion

handlePackage : String -> String -> (Maybe Version) -> Promise Response
handlePackage packageNS packageName maybeVersion =
  do
    Just version <- orLatestVersion maybeVersion packageNS packageName
      | Nothing => pure (404, "not found", [])
    Right pkg <- KVOps.readPackage packageNS packageName version
      | Left err => pure (500, err, [])
    readme <- KVOps.readReadme packageNS packageName version
    versions <- readVersions packageNS packageName
    let vars =
      [ ("NS", ns pkg)
      , ("PACKAGE", package pkg)
      , ("DESCRIPTION", fromMaybe "" $ description pkg)
      , ("EXT_DESCRIPTION", fromMaybe "" $ readme)
      , ("VERSION", show version)
      , ("VERSIONS", versionLinks versions)
      , ("LINK", fromMaybe "" $ map asLink (link pkg))
      , ("LICENSE", fromMaybe "" (license pkg))
      ]
    log ("Vars: " ++ show vars)
    handlePage "/package" vars
  where
    versionLink : Version -> String
    versionLink version = fmt " * [%s](/packages/%s/%s/%s)" (show version) packageNS packageName (show version)

    asLink : String -> String
    asLink link =
      fmt "[%s](%s)" link link

    versionLinks : List Version -> String
    versionLinks =
      join "\n" . map versionLink . reverse . sort

handleStatic : String -> Promise Response
handleStatic path =
  do
    contents <- KV.read "static" path
    let contentType = Util.sniffContentType path
    if contents == ""
      then pure (404, "Static page not found", [])
      else pure (200, contents, contentType)

handleAccountPost : String -> String -> Promise Response
handleAccountPost ns body =
  do
    Right (email, passphrase) <- getAccountValues body
      | Left res => pure res
    salt <- getRand 20
    let kdf = defaultHash salt
    hashedPassphrase <- hash kdf passphrase
    Right account <- lift $ mkAccount ns email kdf passphrase hashedPassphrase
      | Left err => pure (400, err, [])
    Nothing <- KVOps.newAccount account
      | Just (err, msg) => pure (err, msg, [])
    pure (200, "ok", [])
  where
    getAccountValues : String -> Promise (Either Response (String, String))
    getAccountValues body =
      do
        Just toml <- lift $ parseToml body
          | Nothing => pure $ Left (400, "Bad request: invalid body toml", [])
        Just (Str email) <- lift $ get ["email"] toml
          | _ => pure $ Left (400, "Bad request: missing email", [])
        Just (Str passphrase) <- lift $ get ["passphrase"] toml
          | _ => pure $ Left (400, "Bad request: missing passphrase", [])
        pure $ Right (email, passphrase)

handleAccountLoginPost : String -> String -> Promise Response
handleAccountLoginPost ns passphrase =
  do
    Just (kdf, accountHash) <- KVOps.readAccountHash ns
      | _ => pure (404, "not found", [])
    hashedInput <- hash kdf passphrase
    if accountHash == hashedInput
      then do
        session <- getRand 50
        KVOps.writeSession session ns
        pure (200, session, [])
      else
        pure (500, "unauthorized", [])

authenticate : String -> String -> Promise (Maybe Response)
authenticate session ns =
  do
    Just sessionNS <- KVOps.readSession session
      | Nothing => pure $ Just (500, "unauthorized", [])
    if sessionNS == ns
      then
        pure Nothing
      else
        pure $ Just (500, "unauthorized", [])

handleApiArchivePost : String -> String -> String -> Version -> String -> Promise Response
handleApiArchivePost session packageNS packageName version body =
  do
    Nothing <- authenticate session packageNS
      | Just res => pure res
    KVOps.writeArchive packageNS packageName version body
    pure (200, "ok", [])

handleApiReadmePost : String -> String -> String -> Version -> String -> Promise Response
handleApiReadmePost session packageNS packageName version body =
  do
    Nothing <- authenticate session packageNS
      | Just res => pure res
    KVOps.writeReadme packageNS packageName version body
    pure (200, "ok", [])

handleApiPackagePost : String -> String -> String -> Version -> String -> Promise Response
handleApiPackagePost session packageNS packageName version body =
  do
    Nothing <- authenticate session packageNS
      | Just res => pure res
    pkg <- expectResult (parsePackage body)
    -- Update package deps
    currDeps <- KVOps.readDeps pkg
    let nextDeps = updatePackageDep currDeps pkg
    KVOps.writeDeps pkg nextDeps
    -- Update current package
    KVOps.writePackage pkg
    -- Update index if latest
    let isLatest = (Just version) == (head' $ reverse $ sort $ map fst nextDeps) --'
    if isLatest
      then
        do
          -- Update the global index
          currIndex <- KVOps.readIndex
          let nextIndex = updatePackageMeta currIndex pkg
          KVOps.writeIndex nextIndex
      else pure ()
    pure (200, "ok", [])

handleApiPackageConf : String -> String -> Version -> Promise Response
handleApiPackageConf packageNS packageName version =
  do
    Right pkg <- KVOps.readPackage packageNS packageName version
      | Left err => pure (500, err, [])
    pure (200, encodePackage pkg, [])

handleApiPackageDeps : String -> String -> Promise Response
handleApiPackageDeps packageNS packageName =
  do
    deps <- KVOps.readAllDeps packageNS packageName
    log ("Deps: " ++ show deps)
    -- TODO: Better encoding here
    let res = join "\n\n" $ map (\(pkg, deps) => fmt "['%s']\n%s" (join "." pkg) (encodePackageDeps deps)) deps
    pure (200, res, [])

getVersion : String -> Promise Version
getVersion str =
  case parseVersion str of
    Just version =>
      pure version

    Nothing =>
      reject ("Invalid version: " ++ str)

getSession : List (String, String) -> Promise String
getSession headers =
  case map (split ' ' . snd) $ find ((== "authorization") . fst) headers of
    Just ["Basic", session] =>
      pure session
    _ =>
      reject ("Authorization Required")

export
handler : String -> String -> List (String, String) -> List (String, String) -> Promise Response
handler path body params headers =
  do
    log ("Request Path: " ++ path ++ ", params=" ++ show params)
    case drop 1 (split '/' path) of
      ["api", "archive", ns, package, version] =>
        handleApiArchive ns package !(getVersion version)

      ["api", "archive", ns, package, version, "post"] =>
        handleApiArchivePost !(getSession headers) ns package !(getVersion version) body

      ["api", "readme", ns, package, version, "post"] =>
        handleApiReadmePost !(getSession headers) ns package !(getVersion version) body

      ["api", "packages"] =>
        handleApiPackageIndex

      ["api", "packages", ns, package, version, "post"] =>
        handleApiPackagePost !(getSession headers) ns package !(getVersion version) body

      ["api", "packages", ns, package, version, "conf"] =>
        handleApiPackageConf ns package !(getVersion version)

      ["api", "packages", ns, package, "deps"] =>
        handleApiPackageDeps ns package

      ["search"] =>
        case params of
          ("term", term) :: _ =>
            handleSearch term
          _ =>
            pure (404, "Missing search term", [])

      ["packages"] =>
        handlePackageIndex

      ["packages", ns, package] =>
        handlePackage ns package Nothing

      ["packages", ns, package, version] =>
        handlePackage ns package (Just !(getVersion version))

      ["api", "account", ns, "post"] =>
        handleAccountPost ns body

      ["api", "account", ns, "login"] =>
        handleAccountLoginPost ns body

      _ =>
        do
          staticResponse <- handleStatic path
          case staticResponse of
            (200, _, _) =>
              pure $ staticResponse
            _ =>
              handlePage path []
