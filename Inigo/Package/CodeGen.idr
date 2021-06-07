module Inigo.Package.CodeGen

import Data.List.Elem
import Extra.List

-- TODO: When we support external backends use idris2's `CG` for this.
public export
data CodeGen : Type where
  Chez : CodeGen
  ChezSep : CodeGen
  Racket : CodeGen
  Gambit : CodeGen
  Node : CodeGen
  Javascript : CodeGen
  RefC : CodeGen

export
toString : CodeGen -> String
toString Chez = "chez"
toString ChezSep = "chez-sep"
toString Racket = "racket"
toString Gambit = "gambit"
toString Node = "node"
toString Javascript = "javascript"
toString RefC = "refc"

export
getCodeGen : String -> Maybe CodeGen
getCodeGen "chez" = Just Chez
getCodeGen "chez-sep" = Just ChezSep
getCodeGen "racket" = Just Racket
getCodeGen "gambit" = Just Gambit
getCodeGen "node" = Just Node
getCodeGen "javascript" = Just Javascript
getCodeGen "refc" = Just RefC
getCodeGen _ = Nothing

export
cmdArgs : CodeGen -> String -> (String, List String)
cmdArgs Node target = ("node", [target])
cmdArgs Javascript target = ("node", [target])
cmdArgs _ target = (target, [])
