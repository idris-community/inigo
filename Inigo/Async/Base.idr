module Inigo.Async.Base

import Data.Maybe
import Inigo.Async.Promise
import Inigo.Async.Util
import Inigo.Paths

%foreign (promisifyPrim "()=>new Promise((resolve,reject)=>{})")
never__prim : promise ()

%foreign (promisifyPrim "(_,err)=>new Promise((resolve,reject)=>reject(err))")
reject__prim : String -> promise a

%foreign (promisifyResolve "null" "(text)=>console.log(text)")
log__prim : String -> promise ()

%foreign (promisifyPrim (toArray "(cmd,args,workDir,detached,verbose)=>new Promise((resolve,reject)=>{let opts={detached:detached===1n, stdio: ['ignore', process.stdout, process.stderr],cwd:workDir};require('child_process').spawn(cmd, toArray(args), opts).on('close', (code) => resolve(BigInt(code)))})"))
system__prim : String -> List String -> String -> Int -> Int -> promise Int

%foreign (promisifyPrim (toArray "(cmd,args,detached,verbose)=>new Promise((resolve,reject)=>{let opts={detached:detached===1n, stdio: 'inherit'};require('child_process').spawn(cmd, toArray(args), opts).on('close', (code) => resolve(BigInt(code)))})"))
systemWithStdIO__prim : String -> List String -> Int -> Int -> promise Int

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
debugLog : String -> Promise ()
debugLog text = when DEBUG $ log text

export
system : String -> List String -> Maybe String -> Bool -> Bool -> Promise Int
system cmd args cwd detached verbose =
  promisify (system__prim cmd args (fromMaybe "" cwd) (boolToInt detached) (boolToInt verbose))

export
systemWithStdIO : String -> List String -> Bool -> Bool -> Promise Int
systemWithStdIO cmd args detached verbose =
  promisify (systemWithStdIO__prim cmd args (boolToInt detached) (boolToInt verbose))

-- This is here and not in `Promise.idr` since it relies on `reject`
export
liftEither : Either String a -> Promise a
liftEither x =
  do
    let Right res = x
      | Left err => reject err
    pure res
