module IdrTest.Test

import Data.List
import Data.Maybe
import Data.SortedSet as Set
import Extra.String
import IdrTest.Expectation
import System

data Flag : Type where
  Only : Flag
  Skip : Flag

Eq Flag where
  Only == Only = True
  Skip == Skip = True
  _ == _ = False

Ord Flag where
  compare Only Skip = GT
  compare Skip Only = LT
  compare _ _ = EQ

TestCase : Type
TestCase = (String, () -> Expectation, SortedSet Flag)

export
Test : Type
Test = List TestCase

toTestList : Test -> List TestCase
toTestList = id

runTest : (Nat, List String) -> TestCase -> (Nat, List String)
runTest (succ, failures) (name, expectation, _) =
  case expectation () of
    Nothing =>
      (succ + 1, failures)
    Just err =>
      (succ, (name ++ ": " ++ err) :: failures)

enumerate : Nat -> String -> String -> String
enumerate n single multiple =
  (show n) ++ " " ++ case n of
    1 =>
      single
    _ =>
      multiple

showResults : (Nat, List String) -> Nat -> IO ()
showResults (succ, failures) skips =
  let
    count = succ + List.length failures + skips
    countStr = enumerate count "test" "tests"
    successStr = enumerate succ "success" "successes"
    failureStr = enumerate (List.length failures) "failure" "failures"
    skipStr = enumerate skips "test skipped" "tests skipped"
    allFailuresStr = join "\n\n" $ map (\f => "❌ Failed: " ++ f) failures
  in
    do putStrLn allFailuresStr
       putStrLn (join ", " [countStr, successStr, failureStr, skipStr])

export
describe : String -> (List Test) -> Test
describe desc =
  map (\(name, expectation) => (desc ++ ": " ++ name, expectation)) . concat

export
only : Test -> Test
only =
  map (\(name, expectation, flags) => (name, expectation, insert Only flags))

export
skip : Test -> Test
skip =
  map (\(name, expectation, flags) => (name, expectation, insert Skip flags))

export
test : String -> (() -> Expectation) -> Test
test name expectation =
  [
    (name, expectation, Set.empty)
  ]

filterTests : List TestCase -> (List TestCase, List TestCase)
filterTests tests =
  let
    cond = if hasOnly tests
      then filterForOnly
      else filterSkips
  in
    partition cond tests
  where
    hasOnly : List TestCase -> Bool
    hasOnly =
      any (\(_, _, f) => Set.contains Only f)

    filterSkips : TestCase -> Bool
    filterSkips (_, _, flags) =
      not $ Set.contains Skip flags

    filterForOnly : TestCase -> Bool
    filterForOnly (_, _, flags) =
      Set.contains Only flags && (not $ Set.contains Skip flags)

export
runSuite : Test -> IO Bool
runSuite suite =
  let
    (tests, skips) = filterTests (toTestList suite)
    results = foldl runTest (0, []) tests
  in
    do
      showResults results (List.length skips)
      pure $ ((length . snd) results > 0) -- has failures

export
runSuites : List Test -> IO ()
runSuites tests =
  do
    hasFailures <- (runSuite . concat) tests
    if hasFailures
      then exitFailure
      else exitSuccess
