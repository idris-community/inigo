module Fmt

import Data.String

%default total

-- From https://gist.github.com/chrisdone/672efcd784528b7d0b7e17ad9c115292

-- TODO: Add showables

public export
data Format
  = FInt Format
  | FString Format
  | FOther Char Format
  | FEnd

public export
format : List Char -> Format
format ('%' :: 'd' :: cs) = FInt (format cs)
format ('%' :: 's' :: cs) = FString (format cs)
format (c :: cs) = FOther c (format cs)
format [] = FEnd

public export
formatString : String -> Format
formatString = format . unpack

public export
interpFormat : Format -> Type
interpFormat (FInt f) = Int -> interpFormat f
interpFormat (FString f) = String -> interpFormat f
interpFormat (FOther _ f) = interpFormat f
interpFormat FEnd = String

public export
toFunction : (fmt : Format) -> String -> interpFormat fmt
toFunction (FInt f) a = \i => toFunction f (a ++ show i)
toFunction (FString f) a = \s => toFunction f (a ++ s)
toFunction (FOther c f) a = toFunction f (a ++ singleton c)
toFunction FEnd a = a

public export
fmt : (s : String) -> interpFormat (formatString s)
fmt s = toFunction (formatString s) ""
