module Client.Action.FetchDeps

import Client.Action.BuildDeps
import Client.Action.Pull
import Client.Server
import Data.List
import Data.SortedSet as Set
import Extra.String
import Inigo.Async.Base
import Inigo.Async.Fetch
import Inigo.Async.Package
import Inigo.Async.Promise
import Inigo.Package.Package
import Inigo.Package.PackageDeps
import Inigo.Util.Url.Url
import SemVar
import SemVar.Sat

getPackageDepTree : Server -> String -> String -> Promise PackageDepTree
getPackageDepTree server packageNS packageName =
  do
    let url = toString (fromHostPath (host server) (getPackageDepTreeUrl packageNS packageName))
    log ("Requesting " ++ url ++ "...")
    contents <- fetch url
    Right packageDepTree <- lift (parsePackageDepTree contents)
      | Left err => reject ("Invalid package dep tree: " ++ err)
    pure packageDepTree

-- Responsible for pulling deps based on package conf

-- Note: we're currently going to ignore sub-deps entirely

splitDep : List String -> Maybe (String, String)
splitDep (ns :: dep) = Just (ns, join "." dep)
splitDep _ = Nothing

removeDups : Ord a => List a -> List a
removeDups = Set.toList . Set.fromList

collect : Bool -> PackageDepTree -> List VersionNode
collect includeDevDeps depTree =
  concat $ map (\(pkg, pkgDeps) =>
    map (\(version, pkgDep) =>
      (pkg, version, (deps pkgDep) ++ (if includeDevDeps then (dev pkgDep) else []))
    ) pkgDeps
  ) depTree

-- Okay, here's where the fun begins
-- We're going to grab all sub-dep trees via our new deps endpoint
-- and then we're going to cull dups and then pass it to semvar sat to try
-- and return us what deps we should fetch!
export
fetchDeps : Server -> Bool -> Bool -> Promise ()
fetchDeps server includeDevDeps build =
  do
    pkg <- Inigo.Async.Package.currPackage
    -- We have a list of deps, so we basically just need to `pull` each
    -- but we need to know the versions...
    -- Let's start by just pulling the latest of each
    let allDeps = deps pkg ++ (if includeDevDeps then (devDeps pkg) else [])
    let depNames = map fst allDeps
    let splitDeps = mapMaybe splitDep depNames

    depTree <- all $ map (uncurry $ getPackageDepTree server) splitDeps
    let versionNodes = collect includeDevDeps (concat depTree)
    log ("Package Dep Tree: " ++ (show versionNodes))
    Right sat <- lift $ satisfyAll versionNodes allDeps
      | Left err => reject ("Error satisfying contraints: " ++ err)
    log ("Sat: " ++ (show sat))
    
    ignore $ all $ map pullDep sat
    -- TODO: We should only build things which have changed
    -- TODO: How do we know what's changed?
    -- if build
    --   then buildDeps
    --   else pure ()
  where
    pullDep : (List String, Version) -> Promise ()
    pullDep (pkg, version) =
      case splitDep pkg of
        Nothing =>
          reject ("Invalid dep: " ++ show pkg)

        Just (packageNS, packageName) =>
          do
            pull server packageNS packageName (Just version)
