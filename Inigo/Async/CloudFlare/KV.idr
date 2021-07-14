module Inigo.Async.CloudFlare.KV

import Inigo.Async.Promise
import Inigo.Async.Util

%foreign (promisifyPrim "(ns,key)=>this[ns] ? this[ns].get(key).then((r) => r || '') : Promise.reject('Unknown KV namespace ' + ns)")
read__prim : String -> String -> promise String

%foreign (promisifyPrim "(ns,key,value)=>this[ns] ? this[ns].put(key,value) : Promise.reject('Unknown KV namespace ' + ns)")
write__prim : String -> String -> String -> promise ()

%foreign (promisifyPrim "(ns,key,value,expirationTtl)=>this[ns] ? this[ns].put(key,value,{expirationTtl: expirationTtl}) : Promise.reject('Unknown KV namespace ' + ns)")
writeTTL__prim : String -> String -> String -> Int -> promise ()

export
read: String -> String -> Promise String
read ns key =
  promisify (read__prim ns key)

export
write: String -> String -> String -> Promise ()
write ns key value =
  promisify (write__prim ns key value)

export
writeTTL: String -> String -> String -> Int -> Promise ()
writeTTL ns key value ttl =
  promisify (writeTTL__prim ns key value ttl)
