module Inigo.Async.SubtleCrypto.SubtleCrypto

import Inigo.Async.Base
import Inigo.Async.Promise
import Inigo.Async.Util

public export
data Usage : Type where
  Sign : Usage
  Verify : Usage
  (<&>) : Usage -> Usage -> Usage

infixr 5 <&>

usageArray : Usage -> List String
usageArray Sign = ["sign"]
usageArray Verify = ["verify"]
usageArray (u0 <&> u1) = (usageArray u0) ++ (usageArray u1)

interface Encodable a where
  toAnyPtr : a -> AnyPtr

public export
data EllipticCurve : Type where
  P256 : EllipticCurve
  P384 : EllipticCurve
  P512 : EllipticCurve

encodeCurve : EllipticCurve -> String
encodeCurve P256 = "P-256"
encodeCurve P384 = "P-384"
encodeCurve P512 = "P-512"

public export
data KeyAlgorithm : Type where
  KeyECDSA : EllipticCurve -> KeyAlgorithm
  KeyECDH : EllipticCurve -> KeyAlgorithm

public export
data SHA : Type where
  Sha256 : SHA
  Sha384 : SHA
  Sha512 : SHA

public export
Eq SHA where
  Sha256 == Sha256 = True
  Sha384 == Sha384 = True
  Sha512 == Sha512 = True
  _ == _ = False

export
encodeSha : SHA -> String
encodeSha Sha256 = "SHA-256"
encodeSha Sha384 = "SHA-384"
encodeSha Sha512 = "SHA-512"

export
decodeSha : String -> Maybe SHA
decodeSha "SHA-256" = Just Sha256
decodeSha "SHA-384" = Just Sha384
decodeSha "SHA-512" = Just Sha512
decodeSha _ = Nothing

public export
data Algorithm : Type where
  ECDSA : SHA -> Algorithm
  PBKDF2 : SHA -> String -> Int -> Algorithm

public export
Eq Algorithm where
  (ECDSA sha0) == (ECDSA sha1) = sha0 == sha1
  (PBKDF2 sha0 salt0 iterations0) == (PBKDF2 sha1 salt1 iterations1) = sha0 == sha1 && salt0 == salt1 && iterations0 == iterations1
  _ == _ = False

Key : Type
Key = AnyPtr
-- data Key : Type where
--   Key : AnyPtr -> Key

-- Encodable Key where
--   toAnyPtr (Key ptr) = ptr

data EncAlgo : Type where
  MkEncAlgo : AnyPtr -> EncAlgo

Encodable EncAlgo where
  toAnyPtr (MkEncAlgo ptr) = ptr

data EncKeyAlgo : Type where
  MkEncKeyAlgo : AnyPtr -> EncKeyAlgo

Encodable EncKeyAlgo where
  toAnyPtr (MkEncKeyAlgo ptr) = ptr

Signature : Type
Signature = String

inspect : String -> String
inspect str = "(function(){let inspect=(r,msg)=>{console.log(msg, r); return r;};return " ++ str ++ "})()"

arrayBufferEnc64 : String -> String
arrayBufferEnc64 str = "(function(){let arrayBufferEnc64=(buf)=>{let string = ''; (new Uint8Array(buf)).forEach((byte) => { string += String.fromCharCode(byte) }); return btoa(string);};return " ++ str ++ "})()"

arrayBufferDec64 : String -> String
arrayBufferDec64 str = "(function(){let arrayBufferDec64=(string)=>{string = atob(string); const length = string.length, buf = new ArrayBuffer(length), bufView = new Uint8Array(buf); for (var i = 0; i < length; i++) { bufView[i] = string.charCodeAt(i) } return buf;};return " ++ str ++ "})()"

%foreign "node:lambda:" ++ (arrayBufferDec64 "(name, hash, salt, iterations) => ({name, hash, salt: salt ? arrayBufferDec64(salt) : null, iterations: iterations ? Number(iterations) : null})")
encodeAlgorithm__prim : String -> String -> String -> Int -> PrimIO AnyPtr

%foreign "node:lambda:(name, namedCurve) => ({name, namedCurve})"
encodeKeyAlgorithm__prim : String -> String -> PrimIO AnyPtr

%foreign (promisifyPrim (toArray "(algo, usage, encKey) => crypto.subtle.importKey(\"jwk\", JSON.parse(encKey), algo, false, toArray(usage))"))
importKey__prim : AnyPtr -> List String -> String -> promise Key

%foreign (promisifyPrim "(key) => crypto.subtle.exportKey(\"jwk\", key).then((e) => JSON.stringify(e))")
exportKey__prim : Key -> promise String

%foreign (promisifyPrim (toArray "(algo, usage) => crypto.subtle.generateKey(algo, true, toArray(usage)).then((k) => [k.publicKey, k.privateKey]).then(__prim_js2idris_array)"))
generateKey__prim : AnyPtr -> List String -> promise (List AnyPtr)

%foreign (promisifyPrim (arrayBufferEnc64 "(algo, key, data) => crypto.subtle.sign(algo, key, new TextEncoder().encode(data)).then(arrayBufferEnc64)"))
sign__prim : AnyPtr -> AnyPtr -> String -> promise String

%foreign (promisifyPrim (arrayBufferDec64 "(algo, key, signature, data) => crypto.subtle.verify(algo, key, arrayBufferDec64(signature), new TextEncoder().encode(data)).then((res) => res ? 1n : 0n)"))
verify__prim : AnyPtr -> AnyPtr -> String -> String -> promise Int

%foreign ("node:lambda:" ++ (arrayBufferEnc64 "(len) => arrayBufferEnc64(crypto.getRandomValues(new Uint8Array(Number(len))))"))
getRand__prim : Int -> PrimIO String

%foreign (promisifyPrim (inspect (arrayBufferEnc64 "(algo, password) => crypto.subtle.importKey('raw', new TextEncoder().encode(password), algo, false, ['deriveBits', 'deriveKey']).then((km) => crypto.subtle.deriveKey(algo, km, {name:'AES-CBC','length':256}, true, ['encrypt', 'decrypt']).then((k) => crypto.subtle.exportKey('raw', k).then(arrayBufferEnc64)))")))
hash__prim : AnyPtr -> String -> promise String

encodeAlgorithm : Algorithm -> Promise EncAlgo
encodeAlgorithm (ECDSA sha) =
  map MkEncAlgo $ liftIO $ primIO (encodeAlgorithm__prim "ECDSA" (encodeSha sha) "" 0)

encodeAlgorithm (PBKDF2 sha salt iterations) =
  map MkEncAlgo $ liftIO $ primIO (encodeAlgorithm__prim "PBKDF2" (encodeSha sha) salt iterations)

encodeKeyAlgorithm : KeyAlgorithm -> Promise EncKeyAlgo
encodeKeyAlgorithm (KeyECDSA curve) =
  map MkEncKeyAlgo $ liftIO $ primIO (encodeKeyAlgorithm__prim "ECDSA" (encodeCurve curve))

encodeKeyAlgorithm (KeyECDH curve) =
  map MkEncKeyAlgo $ liftIO $ primIO (encodeKeyAlgorithm__prim "ECDH" (encodeCurve curve))

export
generateKey : KeyAlgorithm -> Usage -> Promise (Key, Key)
generateKey algo usage =
  do
    algoEnc <- encodeKeyAlgorithm algo
    (pub :: priv :: []) <- promisify (generateKey__prim (toAnyPtr algoEnc) (usageArray usage))
      | _ => reject "Invalid key generated"
    pure (pub, priv)

export
exportKey : Key -> Promise String
exportKey key =
  promisify (exportKey__prim key)

export
importKey : KeyAlgorithm -> Usage -> String -> Promise Key
importKey algo usage encKey =
  do
    algoEnc <- encodeKeyAlgorithm algo
    promisify (importKey__prim (toAnyPtr algoEnc) (usageArray usage) encKey)

export
sign : Algorithm -> Key -> String -> Promise String
sign algo key target =
  do
    algoEnc <- encodeAlgorithm algo
    promisify (sign__prim (toAnyPtr algoEnc) key target)    

export
verify : Algorithm -> Key -> String -> String -> Promise Bool
verify algo key sig target =
  do
    algoEnc <- encodeAlgorithm algo
    intToBool <$> promisify (verify__prim (toAnyPtr algoEnc) key sig target)

export
getRand : Int -> Promise String
getRand len =
  do
    liftIO (primIO (getRand__prim len))

export
hash : Algorithm -> String -> Promise String
hash algo password =
  do
    algoEnc <- encodeAlgorithm algo
    promisify (hash__prim (toAnyPtr algoEnc) password)
