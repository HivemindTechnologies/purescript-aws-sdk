module AWS.SecretsManager
  ( SecretsManager
  , makeClient
  , SecretId(..)
  , SecretString(..)
  , getSecretValue
  ) where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))

foreign import data SecretsManager :: Type

foreign import newSecretsManager :: Json -> Effect SecretsManager

makeClient ::
  forall r via.
  Justifiable { | r } { | via } =>
  Fillable { | via } DefaultClientProps =>
  { | r } ->
  Effect SecretsManager
makeClient r = makeClientHelper newSecretsManager props
  where
  viaProxy :: Proxy { | via }
  viaProxy = Proxy

  props :: DefaultClientProps
  props = justifillVia viaProxy r

type InternalGetSecretValueParams
  = { "SecretId" :: String
    }

type InternalGetSecretValueResponse
  = { "Name" :: String
    , "SecretString" :: String
    }

foreign import getSecretValueImpl :: Fn2 SecretsManager InternalGetSecretValueParams (Effect (Promise InternalGetSecretValueResponse))

newtype SecretId
  = SecretId String

derive instance ntSecretId :: Newtype SecretId _

newtype SecretString
  = SecretString String

derive instance ntSecretString :: Newtype SecretString _

type GetSecretValueResponse
  = { name :: SecretId
    , secretString :: SecretString
    }

getSecretValue :: SecretsManager -> SecretId -> Aff GetSecretValueResponse
getSecretValue client (SecretId name) = runFn2 getSecretValueImpl client params # toAffE <#> convert
  where
  params = { "SecretId": name }

  convert internalResponse =
    { name: SecretId internalResponse."Name"
    , secretString: SecretString internalResponse."SecretString"
    }
