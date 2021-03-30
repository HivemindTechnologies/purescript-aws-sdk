module AWS.KMS
  ( makeClient
  , KMS
  , Algorithm(..)
  , Ciphertext(..)
  , Plaintext(..)
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
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..), hush)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Justifill (justifillVia, justifill)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Node.Buffer (Buffer, fromString)
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

newtype Ciphertext
  = Ciphertext Buffer

derive instance nCiphertext :: Newtype Ciphertext _

newtype Plaintext
  = Plaintext Buffer

derive instance nPlaintext :: Newtype Plaintext _

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

type InternalEncryptionInput
  = { "Plaintext" :: Buffer
    , "KeyId" :: String
    , "EncryptionAlgorithm" :: Maybe String
    , "EncryptionContext" :: Maybe Json
    }

type InternalEncryptionOutput
  = { "CiphertextBlob" :: Maybe Buffer
    , "KeyId" :: Maybe String
    , "EncryptionAlgorithm" :: Maybe String
    }

type EncryptionInput
  = { plaintext :: Plaintext
    , keyId :: Arn
    , context :: Maybe Json
    , algorithm :: Maybe Algorithm
    }

type EncryptionOutput
  = { ciphertext :: Maybe Ciphertext
    , keyId :: Maybe Arn
    , algorithm :: Maybe Algorithm
    }

foreign import encryptImpl :: Fn2 KMS InternalEncryptionInput (Effect (Promise InternalEncryptionOutput))

encrypt :: KMS -> EncryptionInput -> Aff EncryptionOutput
encrypt client input = runFn2 encryptImpl client params # toAffE <#> convert
  where
  params =
    { "Plaintext": un Plaintext input.plaintext
    , "KeyId": un Arn input.keyId
    , "EncryptionAlgorithm": input.algorithm <#> toInternal
    , "EncryptionContext": input.context
    }

  -- jInput :: EncryptionInput
  -- jInput = justifill input
  -- params =
  --   { "Plaintext": un Plaintext input.plaintext
  --   , "KeyId": un Arn input.keyId
  --   , "EncryptionAlgorithm": jInput <#> (_.algorithm >>> toInternal)
  --   , "EncryptionContext": jInput <#> (_.context >>> un EncryptionContext)
  --   }
  convert :: InternalEncryptionOutput -> EncryptionOutput
  convert output =
    let
      alg = output."EncryptionAlgorithm" >>= fromInternal >>> hush

      key = output."KeyId" <#> Arn

      cipher = output."CiphertextBlob" <#> Ciphertext
    in
      { keyId: key, algorithm: alg, ciphertext: cipher }

type InternalDecryptionInput
  = { "CiphertextBlob" :: Buffer
    -- , "KeyId" :: Maybe String
    -- , "EncryptionAlgorithm" :: Maybe String
    , "EncryptionContext" :: Nullable Json
    }

type InternalDecryptionOutput
  = { "Plaintext" :: Nullable Buffer
    , "KeyId" :: Nullable String
    , "EncryptionAlgorithm" :: Nullable String
    }
{-
{ KeyId:
   'arn:aws:kms:eu-central-1:346347395287:key/9290ffd1-3eb7-4cad-9345-61c1866b5750',
  Plaintext:
   <Buffer 1d 65 3e c0 f6 47 00 97 a9 59 76 2a b4 b2 c9 0e 01 c4 82 8d 78 d8 d2 df ba c9 9d 23 4b 63 6b 4f>,
  EncryptionAlgorithm: 'SYMMETRIC_DEFAULT' }
-}
type DecryptionInput ec
  = { ciphertext :: Ciphertext -- CiphertextBlob :: String | Buffer | ...
    -- , keyId :: Maybe Arn -- KeyId :: String
    -- , algorithm :: Maybe Algorithm -- EncryptionAlgorithm :: String
    , context :: Maybe ec -- EncryptionContext :: Map String String
    }

type DecryptionOutput
  = { plaintext :: Maybe Plaintext -- Plaintext :: String | Buffer | ...
    , keyId :: Maybe Arn -- KeyId :: String
    , algorithm :: Maybe Algorithm -- EncryptionAlgorithm :: String
    }

foreign import decryptImpl :: Fn2 KMS InternalDecryptionInput (Effect (Promise InternalDecryptionOutput))

decrypt :: forall ec. EncodeJson ec => KMS -> DecryptionInput ec -> Aff DecryptionOutput
decrypt client input = runFn2 decryptImpl client params # toAffE <#> convert
  where
  params =
    { "CiphertextBlob": un Ciphertext input.ciphertext
    -- , "KeyId": input.keyId <#> un Arn
    -- , "EncryptionAlgorithm": input.algorithm <#> toInternal
    , "EncryptionContext": input.context <#> encodeJson # toNullable
    }

  -- _ = spy "params" params
  convert :: InternalDecryptionOutput -> DecryptionOutput
  convert output =
    let
      alg = output."EncryptionAlgorithm" # toMaybe >>= fromInternal >>> hush

      key = output."KeyId" # toMaybe <#> Arn

      plain = output."Plaintext" # toMaybe <#> Plaintext
    in
      { keyId: key, algorithm: alg, plaintext: plain }

-- fromInternal output."EncryptionAlgorithm"
--   <#> \alg -> { plaintext: Plaintext output."Plaintext", keyId: Arn output."KeyId", algorithm: alg }
