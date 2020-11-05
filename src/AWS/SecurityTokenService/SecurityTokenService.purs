module AWS.SecurityTokenService where

import Prelude
import AWS.Core (AccessKeyId(..), Arn(..), Credentials, ExternalId(..), Region(..), SecretAccessKey(..), SessionToken(..))
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn1, Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Newtype (un, class Newtype)
import Data.Nullable (Nullable, toMaybe)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Foreign (Foreign)
import Justifill (justifill)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Record (merge)
import Simple.JSON (class WriteForeign, write, writeImpl)

foreign import data STS :: Type

data StsRegionalEndpoint = Regional | Legacy
instance writeForeignStsRegionalEndpoint :: WriteForeign StsRegionalEndpoint where
  writeImpl Regional = writeImpl "regional"
  writeImpl Legacy = writeImpl "legacy"

type PropsR
  = ( accessKeyId :: Maybe AccessKeyId
    , secretAccessKey :: Maybe SecretAccessKey
    , region :: Maybe Region
    , sessionToken :: Maybe SessionToken
    , stsRegionalEndpoint :: Maybe StsRegionalEndpoint
    )

type Props = Record PropsR

foreign import makeClientImpl :: Foreign -> Effect STS

makeClientHelper :: Props -> Effect STS
makeClientHelper = write >>> makeClientImpl

makeClient ::
  forall t3 t4 t5 t6 t7.
  RowToList t5 t6 =>
  FillableFields t6 () t5 =>
  Union
    t3
    t5
    PropsR =>
  RowToList t4 t7 =>
  JustifiableFields t7 t4 () t3 =>
  Record t4 ->
  Effect STS
makeClient r = ((justifill r) :: Props) # makeClientHelper

makeRegionalClient = merge { stsRegionalEndpoint: Just Regional } >>> makeClient

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
