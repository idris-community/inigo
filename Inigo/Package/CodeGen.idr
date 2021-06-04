module Inigo.Package.CodeGen

import Core.Options

public export
data CodeGen : Type where
    MkCodeGen : CG -> CodeGen

public export
Node : CodeGen
Node = MkCodeGen Node

export
cmdArgs : CodeGen -> String -> (String, List String)
cmdArgs (MkCodeGen cg) target = (show cg, [target])

export
toString : CodeGen -> String
toString (MkCodeGen cg) = show cg

-- TODO: Invert map?
export
getCodeGen : String -> CodeGen
getCodeGen "chez" = MkCodeGen $ Chez
getCodeGen "chez-sep" = MkCodeGen $ ChezSep
getCodeGen "racket" = MkCodeGen $ Racket
getCodeGen "node" = MkCodeGen $ Node
getCodeGen "javascript" = MkCodeGen $ Javascript
getCodeGen "refc" = MkCodeGen $ RefC
getCodeGen "gambit" = MkCodeGen $ Gambit
getCodeGen cg = MkCodeGen $ Other cg
