module Test.Inigo.Account.AccountTest

import IdrTest.Test
import IdrTest.Expectation

import Inigo.Account.Account
import Inigo.Async.SubtleCrypto.SubtleCrypto

simpleAccount : Account
simpleAccount =
  MkAccount "tom" "tom@example.com" (PBKDF2 Sha512 "salt" 1000) "aaa"

simpleAccountToml : String
simpleAccountToml ="""
ns=\"tom\"
email=\"tom@example.com\"
kdf.type=\"PBKDF2\"
kdf.sha=\"SHA-512\"
kdf.salt=\"salt\"
kdf.iterations=1000
hash=\"aaa\"
"""

export
suite : Test
suite =
  describe "Account" [
    test "Account Encode" (\_ => assertEq
      (encode simpleAccount)
      simpleAccountToml
    ),
    test "Simple Decode" (\_ => assertEq
      (decode simpleAccountToml)
      (Just simpleAccount)
    )
  ]
