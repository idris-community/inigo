module Toml

import Data.List
import Data.String
import Extra.String
import Toml.Parser
import Toml.Lexer

import public Toml.Data

||| Parse a Toml document
export
parseToml : String -> Maybe Toml
parseToml x = parseTomlToks !(lexToml x)

||| Encode a Toml document with raw keys
export
encode : Toml -> String
encode =
  join "\n" . map showVal
  where
    showKeyPart : String -> String
    showKeyPart k =
      if includesAny ".=\"'" k
        then quote k
        else k

    showKey : List String -> String
    showKey =
      join "." . map showKeyPart

    showVal : (List String, Value) -> String
    showVal (key, ArrTab l) =
      let
        heading = (++) ("[[" ++ (showKey key) ++ "]]\n")
        contents = map (heading . encode) l
      in
        join "\n\n" contents

    showVal (key, value) =
      let
        k = showKey key
        v = show value
      in
        k ++ "=" ++ v

export
get : List String -> Toml -> Maybe Value
get key =
  (map snd) . (find ((== key) . fst))

export
getToml : List String -> Toml -> Toml
getToml key =
  (map (mapFst (drop (length key)))) . (filter (isPrefixOf key . fst))
