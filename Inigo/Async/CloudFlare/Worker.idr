module Inigo.Async.CloudFlare.Worker

import Data.Buffer
import Data.List
import Inigo.Async.Promise

public export
Headers : Type
Headers = List (String, String)

public export
Response : Type
Response = (Int, String, Headers)

export
html : String -> Response
html content =
  (200, content, [("Content-Type", "text/html")])

-- Could maybe make use of `__prim_js2idris_array`
%foreign "javascript:lambda:(status,body,headers)=>{let toArray=((list)=>Object.fromEntries([...Array(list.h).keys()].map((i)=>{let x=list['a'+(i+1)];return [x.a1,x.a2]})));return new Response(body, {status: status, headers: toArray(headers)});}"
prim__response : Int -> String -> List (String, String) -> PrimIO ()

-- Note: can't use "Buffer" body here, needs to be an ArrayBuffer
%foreign "javascript:lambda:(handler,w)=>addEventListener('fetch',e=>e.respondWith(e.request.text().then((r)=>handler((url=new URL(e.request.url)).pathname)(r || '')(__prim_js2idris_array([...url.searchParams.entries()].flat()))(__prim_js2idris_array([...e.request.headers.entries()].flat()))(w))))"
prim__httpServer : (String -> String -> List String -> List String -> IO ()) -> PrimIO ()

handleResponse : (Int, String, Headers) -> PrimIO ()
handleResponse (status, err, headers) =
  prim__response status err headers

primHandler : Promise Response -> IO ()
primHandler handler =
  resolve handler (primIO . handleResponse) (primIO . (\err => handleResponse (500, err, [])))

pairList : List a -> List (a, a)
pairList =
  fst . foldl accumulate ([], Nothing) . Data.List.reverse
  where
    accumulate : (List (a, a), Maybe a) -> a -> (List (a, a), Maybe a)
    accumulate (acc, Nothing) x = (acc, Just x)
    accumulate (acc, Just y) x = ((x, y) :: acc, Nothing)

httpServer : (String -> String -> List (String, String) -> List (String, String) -> Promise Response) -> Promise ()
httpServer handler =
  do
    liftIO $ (primIO $ prim__httpServer ioHandler)
    pure ()
  where
    -- Not sure why this wouldn't compile in point-free style
    ioHandler : String -> String -> List String -> List String -> IO ()
    ioHandler path body params headers = primHandler (handler path body (pairList params) (pairList headers))

export
serve : (String -> String -> List (String, String) -> List (String, String) -> Promise Response) -> Promise ()
serve handler =
  httpServer handler
