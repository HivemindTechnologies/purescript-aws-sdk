module AWS.KMS
  ( makeClient
  , KMS
  , EncryptionParams
  , Algorithm(..)
  , Ciphertext
  , Plaintext
  , EncryptionInput
  , EncryptionOutput
  , DecryptionInput
  , DecryptionOutput
  , decrypt
  , encrypt
  ) where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (Arn(..), DefaultClientProps)
import AWS.Core.Util (raiseEither)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))

foreign import data KMS :: Type

foreign import newKMS :: Json -> Effect KMS

makeClient ::
  forall r via.
  Justifiable { | r } { | via } =>
  Fillable { | via } DefaultClientProps =>
  { | r } ->
  Effect KMS
makeClient r = makeClientHelper newKMS props
  where
  viaProxy :: Proxy { | via }
  viaProxy = Proxy

  props :: DefaultClientProps
  props = justifillVia viaProxy r

type InternalEncryptionParams r
  = ( "KeyId" :: String
    , "EncryptionAlgorithm" :: String
    | r
    )

type InternalPlaintext r
  = ( "Plaintext" :: String | r )

type InternalCiphertext r
  = ( "CiphertextBlob" :: String | r )

type InternalEncryptionInput
  = Record (InternalEncryptionParams (InternalPlaintext ()))

type InternalEncryptionOutput
  = Record (InternalEncryptionParams (InternalCiphertext ()))

data Algorithm
  = SYMMETRIC_DEFAULT
  | RSAES_OAEP_SHA_1
  | RSAES_OAEP_SHA_256

toInternal :: Algorithm -> String
toInternal SYMMETRIC_DEFAULT = "SYMMETRIC_DEFAULT"

toInternal RSAES_OAEP_SHA_1 = "RSAES_OAEP_SHA_1"

toInternal RSAES_OAEP_SHA_256 = "RSAES_OAEP_SHA_256"

fromInternal :: String -> Either String Algorithm
fromInternal "SYMMETRIC_DEFAULT" = Right SYMMETRIC_DEFAULT

fromInternal "RSAES_OAEP_SHA_1" = Right RSAES_OAEP_SHA_1

fromInternal "RSAES_OAEP_SHA_256" = Right RSAES_OAEP_SHA_256

fromInternal alg = Left $ "Unable to parse algorithm " <> alg

type EncryptionParams r
  = ( keyId :: Arn
    , algorithm :: Algorithm
    | r
    )

type Ciphertext r
  = ( ciphertext :: String | r )

type Plaintext r
  = ( plaintext :: String | r )

type EncryptionInput
  = Record (EncryptionParams (Plaintext ()))

type EncryptionOutput
  = Record (EncryptionParams (Ciphertext ()))

foreign import encryptImpl :: Fn2 KMS InternalEncryptionInput (Effect (Promise InternalEncryptionOutput))

encrypt :: KMS -> EncryptionInput -> Aff EncryptionOutput
encrypt client input = runFn2 encryptImpl client params # toAffE <#> convert >>= raiseEither
  where
  params =
    { "KeyId": un Arn input.keyId
    , "EncryptionAlgorithm": toInternal input.algorithm
    , "Plaintext": input.plaintext
    }

  convert :: InternalEncryptionOutput -> Either String EncryptionOutput
  convert internal =
    fromInternal internal."EncryptionAlgorithm"
      <#> \alg -> { keyId: Arn internal."KeyId", algorithm: alg, ciphertext: internal."CiphertextBlob" }

type InternalDecryptionInput
  = Record (InternalEncryptionParams (InternalCiphertext ()))

type InternalDecryptionOutput
  = Record (InternalEncryptionParams (InternalPlaintext ()))

type DecryptionInput
  = Record (EncryptionParams (Ciphertext ()))

type DecryptionOutput
  = Record (EncryptionParams (Plaintext ()))

foreign import decryptImpl :: Fn2 KMS InternalDecryptionInput (Effect (Promise InternalDecryptionOutput))

decrypt :: KMS -> DecryptionInput -> Aff DecryptionOutput
decrypt client input = runFn2 decryptImpl client params # toAffE <#> convert >>= raiseEither
  where
  params =
    { "KeyId": un Arn input.keyId
    , "EncryptionAlgorithm": toInternal input.algorithm
    , "CiphertextBlob": input.ciphertext
    }

  convert :: InternalDecryptionOutput -> Either String DecryptionOutput
  convert internal =
    fromInternal internal."EncryptionAlgorithm"
      <#> \alg -> { keyId: Arn internal."KeyId", algorithm: alg, plaintext: internal."Plaintext" }
