module Inigo.Async.Base

import Inigo.Async.Promise
import Inigo.Async.Util

%foreign (promisifyPrim "()=>new Promise((resolve,reject)=>{})")
never__prim : promise ()

%foreign (promisifyPrim "(_,err)=>new Promise((resolve,reject)=>reject(err))")
reject__prim : String -> promise a

%foreign (promisifyResolve "null" "(text)=>console.log(text)")
log__prim : String -> promise ()

%foreign (promisifyPrim (toArray "(cmd,args,detached,verbose)=>new Promise((resolve,reject)=>{let opts={detached:detached===0n, stdio: ['ignore', process.stdout, process.stderr]};require('child_process').spawn(cmd, toArray(args), opts).on('close', (code) => resolve(code))})"))
system__prim : String -> List String -> Bool -> Bool -> promise Int

export
never : Promise ()
never =
  promisify never__prim

export
reject : String -> Promise a
reject err =
  promisify (reject__prim err)

export
log : String -> Promise ()
log text =
  promisify (log__prim text)

export
system : String -> List String -> Bool -> Bool -> Promise Int
system cmd args detached verbose =
  promisify (system__prim cmd args detached verbose)

-- This is here and not in `Promise.idr` since it relies on `reject`
export
liftEither : Either String a -> Promise a
liftEither x =
  do
    Right res <- lift x
      | Left err => reject err
    pure res
