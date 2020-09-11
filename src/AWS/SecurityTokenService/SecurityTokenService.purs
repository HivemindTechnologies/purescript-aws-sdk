module AWS.SecurityTokenService where

import Prelude
import AWS.Core (AccessKeyId(..), Arn(..), Credentials, ExternalId(..), Region(..), SecretAccessKey(..), SessionToken(..))
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn1, Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Newtype (un, class Newtype)
import Data.Nullable as Nullable
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

foreign import data STS :: Type

type InternalMakeClientParams
  = { region :: String
    , secretAccessKey :: String
    , accessKeyId :: String
    }

foreign import makeClientImpl :: Fn1 InternalMakeClientParams (Effect STS)

foreign import makeDefaultClientImpl :: Effect STS

makeClient :: Region -> AccessKeyId -> SecretAccessKey -> Effect STS
makeClient r a s =
  makeClientImpl
    { region: un Region r
    , secretAccessKey: un SecretAccessKey s
    , accessKeyId: un AccessKeyId a
    }

makeDefaultClient :: Effect STS
makeDefaultClient = makeDefaultClientImpl

type InternalAssumeRoleParams
  = { "RoleArn" :: String
    , "ExternalId" :: Nullable String
    , "RoleSessionName" :: String
    }

type InternalAssumeRoleResponse
  = { "Credentials" :: Nullable InternalCredentials
    , "AssumedRoleUser" :: Nullable InternalAssumedRoleUser
    }

type InternalCredentials
  = { "AccessKeyId" :: String
    , "SecretAccessKey" :: String
    , "SessionToken" :: String
    }

type InternalAssumedRoleUser
  = { "AssumedRoleId" :: String
    , "Arn" :: String
    }

newtype RoleSessionName
  = RoleSessionName String

derive instance ntRoleSessionName :: Newtype RoleSessionName _

foreign import assumeRoleImpl :: Fn2 STS InternalAssumeRoleParams (Effect (Promise InternalAssumeRoleResponse))

assumeRole :: STS -> Arn -> Maybe ExternalId -> RoleSessionName -> Aff Credentials
assumeRole sts roleArn externalId roleSessionName = toExternal =<< Promise.toAffE curried
  where
  toCredentials :: InternalAssumeRoleResponse -> Maybe Credentials
  toCredentials r =
    toMaybe r."Credentials"
      <#> \creds ->
          { accessKeyId: (AccessKeyId creds."AccessKeyId")
          , secretAccessKey: (SecretAccessKey creds."SecretAccessKey")
          , sessionToken: (SessionToken creds."SessionToken")
          }

  toExternal :: InternalAssumeRoleResponse -> Aff Credentials
  toExternal i =
    liftEffect case toCredentials i of
      Just credentials -> pure credentials
      Nothing -> throw "Failed to assume role"

  params :: InternalAssumeRoleParams
  params =
    { "RoleArn": un Arn roleArn
    , "ExternalId": Nullable.toNullable $ externalId <#> un ExternalId
    , "RoleSessionName": un RoleSessionName roleSessionName
    }

  curried = runFn2 assumeRoleImpl sts params
