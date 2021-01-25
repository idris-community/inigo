module Inigo.Async.Fetch

import Data.Buffer
import Extra.Buffer
import Inigo.Async.Base
import Inigo.Async.Promise
import Inigo.Async.Util

%foreign (promisifyPrim "(url)=>new Promise((resolve,reject)=>(url.startsWith('https')?require('https'):require('http')).get(url,(r)=>{let d='';r.on('data',(c)=>{d+=c;});r.on('end',()=>{resolve(d)});}).on('error',(e)=>{reject(e);}))")
fetch__prim : String -> promise String

%foreign (promisifyPrim "(url)=>new Promise((resolve,reject)=>(url.startsWith('https')?require('https'):require('http')).get(url,(r)=>{let cs=[];r.on('data',(c)=>{cs.push(Buffer.from(c))});r.on('end',()=>{resolve(Buffer.concat(cs))});}).on('error',(e)=>{reject(e);}))")
fetchBuf__prim : String -> promise Buffer

%foreign (promisifyPrim ("(url,method,data,headers)=>new Promise((resolve,reject)=>{"++ toObject ++";let u=new URL(url);let port=u.port!==''?u.port:u.protocol==='https:'?443:80;let opts={hostname:u.hostname,port,path:u.pathname,method,headers:toObject(headers)};let req=(u.protocol==='https:'?require('https'):require('http')).request(opts,(r)=>{let cs=[];r.on('data',(c)=>{cs.push(Buffer.from(c))});r.on('end',()=>{resolve(__prim_js2idris_array([Buffer.from(r.statusCode.toString(), 'utf-8'), Buffer.concat(cs)]))});}).on('error',(e)=>reject(e));req.on('error',(e)=>reject(e));req.write(data);req.end();})"))
request__prim : String -> String -> Buffer -> List (String, String) -> promise (List Buffer)

export
fetch : String -> Promise String
fetch url =
  promisify (fetch__prim url)

export
fetchBuf : String -> Promise Buffer
fetchBuf url =
  promisify (fetchBuf__prim url)

convertStatusCode : List Buffer -> Promise (Int, Buffer)
convertStatusCode lb =
  case lb of
    statusBuf :: buf :: [] =>
      do
        status <- liftIO $ readAll statusBuf
        pure (cast status, buf)
    _ =>
      reject "Invalid response"

export
requestBuf : String -> String -> Buffer -> List (String, String) -> Promise (Int, Buffer)
requestBuf url method body headers =
  do
    res <- promisify (request__prim url method body headers)
    convertStatusCode res

export
request : String -> String -> String -> List (String, String) -> Promise (Int, Buffer)
request url method body headers =
  do
    buf <- liftIO $ fromString body
    res <- promisify (request__prim url method buf headers)
    convertStatusCode res
