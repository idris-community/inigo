module Server.InigoServer.KVOps

import Inigo.Async.Promise

import Data.List
import Data.Maybe
import Data.SortedSet as Set
import Extra.String
import Fmt
import Inigo.Account.Account as Account
import Inigo.Async.Base
import Inigo.Async.CloudFlare.KV
import Inigo.Async.SubtleCrypto.SubtleCrypto
import Inigo.Package.Package
import Inigo.Package.PackageDeps
import Inigo.Package.PackageIndex
import SemVar.Data
import Server.InigoServer.Util

||| TODO: Should package technically be a list of strings?
||| TODO: We should be able to rebuild if any of the redundant stores becomes corrupted

sessionTTL : Int
sessionTTL = 60 * 60 * 24 * 10 -- 10 days

packageIndexKey : (String, String)
packageIndexKey = ("index", "index")

packageKey : String -> String -> Version -> (String, String)
packageKey ns package version = ("packages", fmt "%s.%s@%s" ns package (show version))

readmeKey : String -> String -> Version -> (String, String)
readmeKey ns package version = ("readme", fmt "%s.%s@%s" ns package (show version))

archiveKey : String -> String -> Version -> (String, String)
archiveKey ns package version = ("archives", fmt "%s.%s@%s" ns package (show version))

packageDepKey : String -> String -> (String, String)
packageDepKey ns package = ("deps", fmt "%s.%s" ns package)

accountKey : String -> (String, String)
accountKey ns = ("accounts", ns)

sessionKey : String -> (String, String)
sessionKey session = ("sessions", session)

pkgVerKey : Package -> (String -> String -> Version -> a) -> a
pkgVerKey pkg f =
  f (ns pkg) (package pkg) (version pkg)

pkgKey : Package -> (String -> String -> a) -> a
pkgKey pkg f =
  f (ns pkg) (package pkg)

read : (String, String) -> Promise (Maybe String)
read key =
  do
    res <- (uncurry KV.read key)
    if res == ""
      then pure $ Nothing
      else pure $ Just res

write : (String, String) -> String -> Promise ()
write key val = (uncurry KV.write key) val

writeTTL : (String, String) -> String -> Int -> Promise ()
writeTTL key val ttl = (uncurry KV.writeTTL key) val ttl

export
readIndex : Promise PackageIndex
readIndex =
  do
    indexRes <- read packageIndexKey
    log ("Package Index: " ++ show indexRes)
    expectResult $ fromMaybe (Right []) $ map parsePackageIndex indexRes

export
writeIndex : PackageIndex -> Promise ()
writeIndex index =
  write packageIndexKey (encodePackageIndex index)

export
readArchive : String -> String -> Version -> Promise (Maybe String)
readArchive packageNS packageName version =
  read (archiveKey packageNS packageName version)

export
writeArchive : String -> String -> Version -> String -> Promise ()
writeArchive packageNS packageName version body =
  write (archiveKey packageNS packageName version) body

export
readReadme : String -> String -> Version -> Promise (Maybe String)
readReadme packageNS packageName version =
  read (readmeKey packageNS packageName version)

export
writeReadme : String -> String -> Version -> String -> Promise ()
writeReadme packageNS packageName version body =
  write (readmeKey packageNS packageName version) body

-- TODO: This logic is a little complex to be here
export
readAllDeps : String -> String -> Promise (List (List String, PackageDeps))
readAllDeps packageNS packageName =
  readAllDeps_ Set.empty [packageNS, packageName]
  where
    readDep : List String -> Promise PackageDeps
    readDep [packageNS, packageName] =
      do
        depsRes <- read (packageDepKey packageNS packageName)
        log ("Package Deps: " ++ show depsRes)
        expectResult $ fromMaybe (Right []) $ map parsePackageDeps depsRes

    readDep els =
      reject ("Invalid package name: " ++ show els)

    readAllDeps_ : SortedSet (List String) -> List String -> Promise (List (List String, PackageDeps))
    readAllDeps_ curr q =
      do
        packageDeps <- readDep q
        let allDeps = concat $ map (\(_, packageDep) => deps packageDep ++ dev packageDep) packageDeps
        let subDeps = foldl (\acc, (pkgName, _) => Set.insert pkgName acc) (the (SortedSet (List String)) Set.empty) allDeps
        let newDeps = difference subDeps curr
        let nextSet = Set.insert q curr
        res <- all $ map (readAllDeps_ (union nextSet newDeps)) (Set.toList newDeps)
        pure $ (q, packageDeps) :: (concat res)

export
readDeps : Package -> Promise PackageDeps
readDeps pkg =
  do
    depsRes <- read (pkgKey pkg packageDepKey)
    log ("Package Deps: " ++ show depsRes)
    expectResult $ fromMaybe (Right []) $ map parsePackageDeps depsRes

export
writeDeps : Package -> PackageDeps -> Promise ()
writeDeps pkg packageDeps =
  write (pkgKey pkg packageDepKey) (encodePackageDeps packageDeps)

export
writePackage : Package -> Promise ()
writePackage pkg =
  write (pkgVerKey pkg packageKey) (encodePackage pkg)

export
readPackage : String -> String -> Version -> Promise (Either String Package)
readPackage packageNS packageName version =
  do
    Just contents <- read (packageKey packageNS packageName version)
      | Nothing => pure $ Left "package not found"
    pure $ parsePackage contents

export
readVersions : String -> String -> Promise (List Version)
readVersions packageNS packageName =
  do
    depsRes <- read (packageDepKey packageNS packageName)
    deps <- expectResult $ fromMaybe (Right []) $ map parsePackageDeps depsRes
    pure $ map fst deps

export
latestVersion : String -> String -> Promise (Maybe Version)
latestVersion packageNS packageName =
  do
    versions <- readVersions packageNS packageName
    pure $ head' $ reverse $ sort $ versions --'

-- Note: there could be race conditions, and since this
--       is EV, it would be difficult to prevent overall
-- TODO: Validate account details on server-side
export
newAccount : Account -> Promise (Maybe (Int, String))
newAccount account =
  do
    Nothing <- read (accountKey (ns account))
      | Just _ => pure $ Just (400, "account already exists")
    write (accountKey (ns account)) (encode account)
    pure Nothing

export
readAccountHash : String -> Promise (Maybe (Algorithm, String))
readAccountHash ns =
  do
    Just accountToml <- read (accountKey ns)
      | Nothing => pure Nothing
    let Just account = Account.decode accountToml
      | Nothing => pure Nothing
    pure (Just $ (kdf account, hash account))

export
readSession : String -> Promise (Maybe String)
readSession session =
  read (sessionKey session)

||| TODO: write session should have an expiration
export
writeSession : String -> String -> Promise ()
writeSession session ns =
  writeTTL (sessionKey session) ns sessionTTL
