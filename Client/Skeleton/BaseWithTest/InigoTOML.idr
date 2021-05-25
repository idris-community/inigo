module Client.Skeleton.BaseWithTest.InigoTOML

import Fmt

export
name : (String, String) -> List String
name = const ["Inigo.toml"]

export
build : (String, String) -> String
build (packageNS, packageName) = fmt """
ns=\"%s\"
package=\"%s\"
version=\"0.0.1\"

description=\"\"
link=\"\"
readme=\"\"
modules=[\"%s\"]
depends=[\"idris2\", \"contrib\"]
license=\"\"
main=\"%s\"
executable=\"%s\"

[deps]

[dev-deps]
Base.IdrTest=\"~0.0.1\"
""" packageNS packageName packageName packageName packageName
