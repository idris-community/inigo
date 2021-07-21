module Client.Action.FetchDeps

import Client.Action.BuildDeps
import Client.Action.CacheDeps
import Client.Action.Pull
import Client.Server
import Data.List
import Data.SortedSet as Set
import Extra.String
import Inigo.Async.Base
import Inigo.Async.Fetch
import Inigo.Async.FS
import Inigo.Async.Git
import Inigo.Async.Package
import Inigo.Async.Promise
import Inigo.Package.Package
import Inigo.Package.PackageDeps
import Inigo.Paths
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
fetchDeps : Server -> Bool -> Bool -> Package -> Promise (List (String, Package))
fetchDeps server includeDevDeps build pkg =
  do
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
    
    all $ map pullDep sat
    -- TODO: We should only build things which have changed
    -- TODO: How do we know what's changed?
    -- if build
    --   then buildDeps
    --   else pure ()
  where
    pullDep : (List String, Version) -> Promise (String, Package)
    pullDep (pkg, version) =
      case splitDep pkg of
        Nothing =>
          reject ("Invalid dep: " ++ show pkg)

        Just (packageNS, packageName) =>
          do
            pull server packageNS packageName (Just version)
            let src = inigoDepDir </> joinPath pkg
            pkg <- readPackage $ src </> inigoTomlPath
            pure (src, pkg)


||| Get all elems of the left list not present in the right list
total
difference : Eq a => List a -> List a -> List a
difference xs [] = xs
difference xs (y :: ys) = difference (delete y xs) ys

fetchExtraDeps : Bool -> Bool -> Package -> Promise (List (String, Package))
fetchExtraDeps devDeps build pkg = do
    deps <- fetchDeps [] pkg.extraDeps
    foldlM getExtraDepPkg [] deps
  where
    getSubDirPkg : String -> List (String, Package) -> String -> Promise (List (String, Package))
    getSubDirPkg depDir pkgs subDir = do
        let srcDir = depDir </> subDir
        pkg <- readPackage $ srcDir </> inigoTomlPath
        if any ((== pkg) . snd) pkgs
            then pure pkgs
            else pure ((srcDir, pkg) :: pkgs)

    getExtraDepPkg : List (String, Package) -> ExtraDep -> Promise (List (String, Package))
    getExtraDepPkg pkgs dep@(MkExtraDep _ _ _ subDirs) =
        foldlM (getSubDirPkg $ getExtraDepDir dep) pkgs subDirs

    genIPkg : String -> String -> Promise Package
    genIPkg dest subDir = do
        let buildDir = joinPath (".." <$ splitPath (dest </> subDir)) </> "build"
        let toml = dest </> subDir </> inigoTomlPath
        let iPkgFile = dest </> subDir </> inigoIPkgPath
        pkg <- readPackage toml
        fs_writeFile iPkgFile $ generateIPkg False (Just buildDir) pkg
        pure pkg

    fetchExtraDep : ExtraDep -> Promise (List Package)
    fetchExtraDep pkg@(MkExtraDep Git commit url subDirs) = do
        let dest = getExtraDepDir pkg
        log "Downloading package from \"\{url}\""
        ignore $ git_downloadTo url (Just commit) dest
        traverse (genIPkg dest) subDirs
    fetchExtraDep pkg@(MkExtraDep SubDir _ url subDirs) = do
        let dest = getExtraDepDir pkg
        log "Checking packages in \"\{url}\""
        traverse (genIPkg dest) subDirs

    fetchDeps : List ExtraDep -> List ExtraDep -> Promise (List ExtraDep) -- if this is too slow, use SortedSet
    fetchDeps done [] = pure done
    fetchDeps done (MkExtraDep _ _ _ [] :: todo) = fetchDeps done todo
    fetchDeps done (pkg@(MkExtraDep _ _ _ subDirs0) :: todo) = case find (eqIgnoreSubDirs pkg) done of
        Nothing => do -- doesn't exist yet, download and add dependencies
            pkgs <- fetchExtraDep pkg
            let todo' = foldl (\acc, pkg => pkg.extraDeps ++ acc) todo pkgs
            fetchDeps (pkg :: done) todo'
        Just pkg@(MkExtraDep method info url subDirs1) => case difference subDirs0 subDirs1 of
            [] => fetchDeps done todo -- no missing subdirs, move on
            missing => do
                let dest = getExtraDepDir pkg
                pkgs <- traverse (genIPkg dest) missing
                let todo' = foldl (\acc, pkg => pkg.extraDeps ++ acc) todo pkgs
                let done' = MkExtraDep method info url (missing ++ subDirs0) :: filter (not . eqIgnoreSubDirs pkg) done
                fetchDeps done' todo
export
fetchAllDeps : Server -> Bool -> Bool -> Promise ()
fetchAllDeps server devDeps build = do
    pkg <- currPackage
    deps <- fetchDeps server devDeps build pkg
    extraDeps <- fetchExtraDeps devDeps build pkg
    let allDeps = deps ++ extraDeps
    writeDepCache allDeps
