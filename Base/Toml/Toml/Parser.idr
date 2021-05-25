module Toml.Parser

import Data.List
import Data.List1
import Extra.List
import Text.Parser
import Text.Token
import Toml.Data
import Toml.Lexer

import public Toml.Tokens

comment : Grammar TomlToken True (Maybe (List String, Value))
comment =
  map (const Nothing) (match Comment)

whitespace : Grammar TomlToken True (Maybe (List String, Value))
whitespace =
  map (const Nothing) (match Whitespace)

keyword : Grammar TomlToken True (List String)
keyword =
  forget <$> sepBy1 (match Dot) (match Keyword <|> match StringLit <|> (map show $ match Number))

heading : Grammar TomlToken True (List String)
heading =
  do
    match LeftBracket
    key <- keyword
    match RightBracket
    pure key


doubleHeading : Grammar TomlToken True (List String)
doubleHeading =
  do
    match LeftBracket
    match LeftBracket
    key <- keyword
    match RightBracket
    match RightBracket
    pure key

doubleHeadingOf : List String -> Grammar TomlToken True String
doubleHeadingOf lookup =
  do
    key <- doubleHeading
    the (Grammar _ False _) $
      if key == lookup
        then pure "hi"
        else fail "mismatched double heading"

str : Grammar TomlToken True Value
str =
  map Str (match StringLit)

num : Grammar TomlToken True Value
num =
  map Num (match Number)

optSpacing : Grammar TomlToken True a -> Grammar TomlToken True a
optSpacing inner =
  do
    ignore $ optional (match Whitespace)
    res <- inner
    ignore $ optional (match Whitespace)
    pure res

mutual
  list : Grammar TomlToken True Value
  list =
    do
      ignore $ match LeftBracket
      ignore $ optional (match Whitespace)
      values <- sepBy (optSpacing $ match Comma) value
      ignore $ optional (match Whitespace)
      ignore $ match RightBracket
      pure (Lst values)

  value : Grammar TomlToken True Value
  value =
    str <|> num <|> list

kv : Grammar TomlToken True (Maybe (List String, Value))
kv =
  do
    key <- keyword
    match Equals
    v <- value
    pure $ Just (key, v)

mutual
  subtable : Grammar TomlToken True (List (List String, Value))
  subtable =
    do
      key <- doubleHeading
      -- Let's read a table and repeat with the same heading any number of times
      tomls <- sepBy1 (doubleHeadingOf key) (simpleToml key)
      let val = ArrTab $ forget tomls
      pure [(key, val)]

  emptyHeading : Grammar TomlToken True (List (List String, Value))
  emptyHeading =
    do
      ignore heading
      pure []

  kvs : Grammar TomlToken True (List (List String, Value))
  kvs =
    do
      key <- option [] heading
      inner <- some (
            kv
        <|> comment
        <|> whitespace)
      pure $ map (\(k, v) => (key ++ k, v)) (mapMaybe id $ forget inner)

  simpleToml : List String -> Grammar TomlToken False Toml
  simpleToml pre =
    do
      res <- map concat (many (kvs <|> emptyHeading))
      pure $ map (mapFst $ dropPrefix pre) res

  toml : Grammar TomlToken False Toml
  toml =
    map concat (many (subtable <|> kvs <|> emptyHeading))

export
parseTomlToks : List TomlToken -> Maybe Toml
parseTomlToks toks = case parse toml toks of
                      Right (j, []) => Just j
                      _ => Nothing
