module Extra.String

import Data.List
import Data.List1
import Data.Maybe
import Data.Strings
import Extra.List
import Extra.Op

public export
join : String -> List String -> String
join joiner strs =
  foldl (++) "" (Data.List.intersperse joiner strs)

export
findAll : String -> String -> List Nat
findAll str lookup =
  findAll (unpack str) (unpack lookup)

export
replace : String -> String -> String -> String
replace k v str =
  pack (Extra.List.replace (unpack str) (unpack k) (unpack v))

export
wrap : String -> String -> String
wrap sym str =
  sym ++ str ++ sym

export
filter : (Char -> Bool) -> String -> String
filter test =
  pack . filter test . unpack

export
showList : (a -> String) -> List a -> String
showList f els =
  "[" ++ (join "," (map f els)) ++ "]"

export
listOp : (List Char -> List Char) -> String -> String
listOp op =
  pack . op . unpack

export
split : Char -> String -> List String
split c =
  ( map pack ) . Data.List1.forget . (Data.List.split (== c)) . unpack

export
includesAny : String -> String -> Bool
includesAny search =
  let
    searchChars = unpack search
  in
    isJust . find (\c => c `elem` searchChars) . unpack

export
quote : String -> String
quote =
  (wrap "\"")
  . (replace "\n" "\\n")
  . (replace "\"" "\\\"")

export
unquote : String -> String
unquote =
  (replace "\\n" "\n") .
  (listOp (reverse . drop 1 . reverse . drop 1))

export
limit : Nat -> String -> String
limit len str =
  if length str > len then
    (strSubstr 0 (cast len) str) ++ "..."
  else
    str
