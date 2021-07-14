module Extra.Buffer

import Data.Buffer

%foreign "node:lambda:(buf)=>buf.toString('base64')"
enc64__prim : Buffer -> PrimIO String

%foreign "node:lambda:(str)=>Buffer.from(str, 'base64')"
dec64__prim : String -> PrimIO Buffer

%foreign "node:lambda:(str)=>Buffer.from(str, 'utf-8')"
fromString__prim : String -> PrimIO Buffer

%foreign "node:lambda:(buf,char)=>buf.indexOf(char)"
indexOf__prim : Buffer -> Char -> PrimIO Int

export
enc64 : HasIO io => Buffer -> io String
enc64 =
  primIO . enc64__prim

export
dec64 : HasIO io => String -> io Buffer
dec64 =
  primIO . dec64__prim

export
fromString : HasIO io => String -> io Buffer
fromString =
  primIO . fromString__prim

export
indexOf : HasIO io => Char -> Buffer -> io (Maybe Int)
indexOf char buf =
  do
    res <- primIO (indexOf__prim buf char)
    if res < 0
      then pure $ Nothing
      else pure $ Just res

export
readAll : HasIO io => Buffer -> io String
readAll buf =
  do
    len <- rawSize buf
    x <- getString buf 0 len
    pure x

export
readLine : HasIO io => Buffer -> io (String, Maybe Buffer)
readLine buf =
  do
    Just index <- indexOf '\n' buf
      | Nothing =>  do
                        contents <- readAll buf
                        pure $ (contents, Nothing)
    Just (line, rest) <- splitBuffer buf (index + 1)
      | Nothing => pure $ ( "", Just buf ) -- Shrug
    contents <- readAll line
    pure $ (contents, Just rest)
