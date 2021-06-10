module Inigo.Paths

import public System.Path

export
inigoTomlPath : String
inigoTomlPath = "Inigo.toml"

export
inigoIPkgPath : String
inigoIPkgPath = "Inigo.ipkg"

export
inigoWorkDir : String
inigoWorkDir = ".inigo-work"

export
inigoDepDir : String
inigoDepDir = inigoWorkDir </> "deps"

export
inigoDepPkgCache : String
inigoDepPkgCache = inigoWorkDir </> "deps.cache"

export
DEBUG : Bool
DEBUG = True