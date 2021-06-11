module Inigo.PkgTree

import Data.List
import Data.SortedMap
import Data.SortedSet as Set
import Inigo.Paths

%default total

public export
interface Ord name => HasDeps pkg name where
    getDeps : Bool -> pkg -> List name

-- DEBUG
export
Ord name => HasDeps (List name) name where
    getDeps _ deps = deps

parameters (dev : Bool)

    %inline
    lengthNotIn :
        (done : SortedSet name) ->
        (deps : SortedSet name) ->
        Nat
    lengthNotIn done deps = foldl go 0 deps
    where
        go : Nat -> name -> Nat
        go len dep = if contains dep done
            then len
            else 1 + len

    %inline
    %spec hasDeps
    lookupDeps :
        Show name =>
        (hasDeps : HasDeps pkg name) =>
        SortedMap name pkg ->
        name ->
        Either String (SortedSet name)
    lookupDeps ctxt n = case lookup n ctxt of
        Nothing => Left "Unkown package: \{show n}"
        Just pkg => Right $ fromList $ getDeps dev pkg

    %inline
    %spec hasDeps
    findLeaves :
        Show name =>
        (hasDeps : HasDeps pkg name) =>
        (ctxt : SortedMap name pkg) ->
        List name ->
        (done : SortedSet name) ->
        (todo : SortedSet name) ->
        Either String (List name)
    findLeaves ctxt build done todo = foldlM go [] todo
    where
        go : List name -> name -> Either String (List name)
        go leaves n = do
            deps <- lookupDeps ctxt n
            case lengthNotIn done deps of
                0 => Right $ n :: leaves
                _ => Right leaves

    deleteFrom :
        List a ->
        SortedSet a ->
        SortedSet a
    deleteFrom [] set = set
    deleteFrom (x :: xs) set = deleteFrom xs (delete x set)

    insertFrom :
        List a ->
        SortedSet a ->
        SortedSet a
    insertFrom [] set = set
    insertFrom (x :: xs) set = insertFrom xs (insert x set)

    export
    %spec hasDeps
    kahn :
        Show name =>
        (hasDeps : HasDeps pkg name) =>
        (ctxt : SortedMap name pkg) ->
        (build : List name) ->
        (done : SortedSet name) ->
        (todo : SortedSet name) ->
        Either String (List name)
    kahn ctxt build done todo = if null todo
        then Right $ reverse build
        else do
            leaves@(_ :: _) <- findLeaves ctxt build done todo
                | [] => do
                    let todoList = Set.toList todo
                    deps <- traverse (\pkg => (pkg,) . Set.toList <$> lookupDeps ctxt pkg) todoList
                    if DEBUG
                        then Left "Cyclic dependencies:\n\{show todoList},\ndeps: \{show deps},\nbuild so far: \{show $ reverse build}"
                        else Left "Cyclic dependencies: \{show todoList}"
            kahn ctxt (leaves ++ build) (insertFrom leaves done) (assert_smaller todo $ deleteFrom leaves todo)

    export
    %spec hasDeps
    %inline
    getBuildOrder :
        Show name =>
        (hasDeps : HasDeps pkg name) =>
        (ctxt : SortedMap name pkg) ->
        SortedSet name ->
        Either String (List name)
    getBuildOrder ctxt todo = kahn ctxt [] empty todo
