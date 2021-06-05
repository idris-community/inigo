module SemVar.Parser

import SemVar.Lexer
import SemVar.Data
import Text.Parser
import Text.Token
import Data.List

import public SemVar.Tokens

%default total

release : Grammar SemVarToken True String
release =
  do
    match Hyphen
    match Text

metadata : Grammar SemVarToken True String
metadata =
  do
    match Plus
    match Text

dotOrDefault : Grammar SemVarToken False Int
dotOrDefault =
  option 0 (
    do
      match Dot
      match Number
  )

version : Grammar SemVarToken True Version
version =
  do
    major <- match Number
    minor <- dotOrDefault
    patch <- dotOrDefault
    release <- optional release
    metadata <- optional metadata
    pure (MkVersion major minor patch release metadata)

tilde : Grammar SemVarToken True Requirement 
tilde =
  do
    match Tilde
    v <- version
    pure $ AND (GTE v) (LT $ nextMinor v)

pin : Grammar SemVarToken True Requirement 
pin =
  do
    match Caret
    v <- version
    pure $ case v of
      MkVersion 0 0 patch Nothing Nothing =>
        EQ v
      _ =>
        AND (GTE v) (LT $ nextMajor v)

exact : Grammar SemVarToken True Requirement 
exact =
  do
    ignore $ optional (match CmpEQ)
    v <- version
    pure $ EQ v

gt : Grammar SemVarToken True Requirement 
gt =
  do
    ignore $ optional (match CmpGT)
    v <- version
    pure $ GT v

lt : Grammar SemVarToken True Requirement 
lt =
  do
    ignore $ optional (match CmpLT)
    v <- version
    pure $ LT v

gte : Grammar SemVarToken True Requirement 
gte =
  do
    ignore $ optional (match CmpGTE)
    v <- version
    pure $ GTE v

lte : Grammar SemVarToken True Requirement 
lte =
  do
    ignore $ optional (match CmpLTE)
    v <- version
    pure $ LTE v

range : Grammar SemVarToken True Requirement 
range =
  do
    v0 <- version
    ignore $ optional (match Whitespace)
    match Hyphen
    ignore $ optional (match Whitespace)
    v1 <- version
    pure $ AND (GTE v0) (LTE v1)

simpleRequirement : Grammar SemVarToken True Requirement
simpleRequirement =
  (
        range
    <|> tilde
    <|> pin
    <|> exact
    <|> gte
    <|> gt
    <|> lte
    <|> lt
  )

conj : Grammar SemVarToken True Requirement 
conj =
  do
    v0 <- simpleRequirement
    match Whitespace
    v1 <- simpleRequirement
    pure $ AND v0 v1

disjuction : Grammar SemVarToken True Requirement 
disjuction =
  do
    v0 <- simpleRequirement
    ignore $ optional (match Whitespace)
    ignore $ match Pipe
    ignore $ optional (match Whitespace)
    v1 <- simpleRequirement
    pure $ OR v0 v1

requirement : Grammar SemVarToken True Requirement
requirement =
  (
        conj
    <|> disjuction
    <|> simpleRequirement
  )

export
parseVersionToks : List SemVarToken -> Maybe Version
parseVersionToks toks = case parse version toks of
                      Right (j, []) => Just j
                      _ => Nothing

export
parseRequirementToks : List SemVarToken -> Maybe Requirement
parseRequirementToks toks = case parse requirement toks of
                      Right (j, []) => Just j
                      _ => Nothing
