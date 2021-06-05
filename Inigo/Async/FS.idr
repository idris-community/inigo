module Inigo.Async.FS

import Inigo.Async.Promise
import Inigo.Async.Util
import Data.Buffer
import Extra.Buffer
import System.Path

%foreign (promisifyPrim "(path)=>require('fs').promises.readFile(path,'utf8')")
fs_readFile__prim : String -> promise String

%foreign (promisifyPrim "(path)=>require('fs').promises.readFile(path)")
fs_readFileBuf__prim : String -> promise Buffer

%foreign (promisifyPrim "(path,contents)=>require('fs').promises.writeFile(path,contents)")
fs_writeFile__prim : String -> String -> promise ()

%foreign (promisifyPrim "(path,contents)=>require('fs').promises.writeFile(path,contents)")
fs_writeFileBuf__prim : String -> Buffer -> promise ()

%foreign (promisifyPrim "(path,r)=>require('fs').promises.mkdir(path,{recursive: r === 1n})")
fs_mkdir__prim : String -> Int -> promise ()

%foreign (promisifyPrim "(path)=>require('fs').promises.readdir(path).then(__prim_js2idris_array)")
fs_getFiles__prim : String -> promise (List String)

-- TODO: Include more stat?
%foreign (promisifyPrim "(path)=>require('fs').promises.stat(path).then((s)=>s.isDirectory() ? 1n : 0n)")
fs_stat__prim : String -> promise Int

export
fs_readFile : String -> Promise String
fs_readFile path =
  promisify (fs_readFile__prim path)

export
fs_writeFile : String -> String -> Promise ()
fs_writeFile path contents =
  promisify (fs_writeFile__prim path contents)

export
fs_readFileBuf : String -> Promise Buffer
fs_readFileBuf path =
  promisify (fs_readFileBuf__prim path)

export
fs_writeFileBuf : String -> Buffer -> Promise ()
fs_writeFileBuf path contents =
  promisify (fs_writeFileBuf__prim path contents)

export
fs_mkdir : Bool -> String -> Promise ()
fs_mkdir recursive path =
  promisify (fs_mkdir__prim path (boolToInt recursive))

export
fs_getFiles : String -> Promise (List String)
fs_getFiles path =
  promisify (fs_getFiles__prim path)

export
fs_stat : String -> Promise Bool
fs_stat path =
  intToBool <$> promisify (fs_stat__prim path)

export
fs_getFilesR : String -> Promise (List String)
fs_getFilesR path =
  doGetFilesR path
  where
  doGetFilesR : String -> Promise (List String)
  doGetFilesR path =
    do
      isDir <- fs_stat path
      if isDir
        then do
          entries <- fs_getFiles path
          let fullEntries = map (path </>) entries
          allFiles <- all (map doGetFilesR fullEntries)
          pure (concat allFiles)
        else
          pure [path]
