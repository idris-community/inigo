module Client.Action.Register

import Client.Util
import Client.Server
import Data.Buffer
import Inigo.Async.Fetch
import Inigo.Async.Promise
import Inigo.Util.Url.Url
import Toml

registerAccountCall : Server -> String -> String -> String -> Promise ()
registerAccountCall server ns email passphrase =
  do
    let contents = encode [(["email"], Str email), (["passphrase"], Str passphrase)]
    let url = toString (fromHostPath (host server) (accountPostUrl ns))
    ignore $ assertOk url $ request url "POST" contents []

export
registerAccount : Server -> String -> String -> String -> Promise ()
registerAccount =
  registerAccountCall
