module Toml.Data

import Data.String
import Data.List

import Extra.String

mutual
  public export
  data Value : Type where
    Str : String -> Value
    Num : Int -> Value
    Lst : List Value -> Value
    ArrTab : List Toml -> Value

  public export
  Toml : Type
  Toml = List (List String, Value)

toList : Toml -> List (List String, Value)
toList = id

export
Show Value where
  show (Str str) = (quote str)
  show (Num x) = (show x)
  show (Lst els) = (showList show els)
  show (ArrTab t) = show t

export
Eq Value where
  (Str s0) == (Str s1) = s0 == s1
  (Num x0) == (Num x1) = x0 == x1
  (Lst x0) == (Lst x1) = x0 == x1
  (ArrTab x0) == (ArrTab x1) = x0 == x1
  _ == _ = False
