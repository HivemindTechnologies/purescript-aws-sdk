module AWS.KMS
  ( makeClient
  , KMS
  , Algorithm(..)
  , Ciphertext(..)
  , Plaintext(..)
  , EncryptionContext(..)
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
import Data.Either (Either(..), hush)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Newtype (class Newtype, un)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Justifill (justifillVia, justifill)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))
import Node.Buffer (Buffer, fromString)

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

newtype EncryptionContext
  = EncryptionContext Json

derive instance nEncryptionContext :: Newtype EncryptionContext _

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
    , context :: Maybe EncryptionContext
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
    , "EncryptionContext": input.context <#> un EncryptionContext
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
    , "KeyId" :: Maybe String
    , "EncryptionAlgorithm" :: Maybe String
    , "EncryptionContext" :: Maybe Json
    }

type InternalDecryptionOutput
  = { "Plaintext" :: Maybe Buffer
    , "KeyId" :: Maybe String
    , "EncryptionAlgorithm" :: Maybe String
    }

type DecryptionInput
  = { ciphertext :: Ciphertext -- CiphertextBlob :: String | Buffer | ...
    , keyId :: Maybe Arn -- KeyId :: String
    , algorithm :: Maybe Algorithm -- EncryptionAlgorithm :: String
    , context :: Maybe EncryptionContext -- EncryptionContext :: Map String String
    }

type DecryptionOutput
  = { plaintext :: Maybe Plaintext -- Plaintext :: String | Buffer | ...
    , keyId :: Maybe Arn -- KeyId :: String
    , algorithm :: Maybe Algorithm -- EncryptionAlgorithm :: String
    }

foreign import decryptImpl :: Fn2 KMS InternalDecryptionInput (Effect (Promise InternalDecryptionOutput))

decrypt :: KMS -> DecryptionInput -> Aff DecryptionOutput
decrypt client input = runFn2 decryptImpl client params # toAffE <#> convert
  where
  params =
    { "CiphertextBlob": un Ciphertext input.ciphertext
    , "KeyId": input.keyId <#> un Arn
    , "EncryptionAlgorithm": input.algorithm <#> toInternal
    , "EncryptionContext": input.context <#> un EncryptionContext
    }

  _ = spy "params" params

  convert :: InternalDecryptionOutput -> DecryptionOutput
  convert output =
    let
      alg = output."EncryptionAlgorithm" >>= fromInternal >>> hush

      key = output."KeyId" <#> Arn

      plain = output."Plaintext" <#> Plaintext
    in
      { keyId: key, algorithm: alg, plaintext: plain }

-- fromInternal output."EncryptionAlgorithm"
--   <#> \alg -> { plaintext: Plaintext output."Plaintext", keyId: Arn output."KeyId", algorithm: alg }
