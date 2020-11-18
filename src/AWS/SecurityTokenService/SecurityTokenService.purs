module AWS.SecurityTokenService (STS, StsRegionalEndpoint(..),RoleSessionName(..), STSProps, STSPropsR, makeClient, makeRegionalClient, assumeRole) where

import Prelude

import AWS.Core.Client (makeClientHelper, makeDefaultClient)
import AWS.Core.Types (AccessKeyId(..), Arn(..), BasicClientPropsR, ExternalId(..), SecretAccessKey(..), SessionToken(..), DefaultClientProps)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Newtype (un, class Newtype)
import Data.Nullable (Nullable, toMaybe)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign (Foreign)
import Justifill (justifill)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record (merge)
import Simple.JSON (class WriteForeign, writeImpl)

foreign import data STS :: Type

foreign import newSTS :: Foreign -> Effect STS

data StsRegionalEndpoint = Regional | Legacy

instance writeForeignStsRegionalEndpoint :: WriteForeign StsRegionalEndpoint where
  writeImpl Regional = writeImpl "regional"
  writeImpl Legacy = writeImpl "legacy"

newtype RoleSessionName
  = RoleSessionName String

derive instance ntRoleSessionName :: Newtype RoleSessionName _

type STSPropsR = BasicClientPropsR ( stsRegionalEndpoint :: Maybe StsRegionalEndpoint )

type STSProps = Record STSPropsR

makeClient :: forall t10 t6 t7 t8 t9.
  RowToList t8 t7 => FillableFields t7 () t8 => Union t10 t8
                                                  STSPropsR 
                                                 => RowToList t9 t6 => JustifiableFields t6 t9 () t10 => Record t9 -> Effect STS
makeClient r = ((makeDefaultClient r:: STSProps)) # makeClientHelper newSTS

makeRegionalClient :: forall t49 t53 t54 t55 t56 t57.
  Nub
    ( stsRegionalEndpoint :: Maybe StsRegionalEndpoint
    | t49
    )
    t53
   => RowToList t54 t55 => FillableFields t55 () t54 => Union t57 t54
                                                          STSPropsR
                                                         => RowToList t53 t56 => JustifiableFields t56 t53 () t57 => Record t49 -> Effect STS
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

foreign import assumeRoleImpl :: Fn2 STS InternalAssumeRoleParams (Effect (Promise InternalAssumeRoleResponse))

assumeRole :: STS -> Arn -> Maybe ExternalId -> RoleSessionName -> Aff DefaultClientProps
assumeRole sts roleArn externalId roleSessionName = toExternal =<< Promise.toAffE curried
  where
  toCredentials :: InternalAssumeRoleResponse -> Maybe DefaultClientProps
  toCredentials r =
    toMaybe r."Credentials"
      <#> \creds ->
          justifill { accessKeyId: (AccessKeyId creds."AccessKeyId")
          , secretAccessKey: (SecretAccessKey creds."SecretAccessKey")
          , sessionToken: (SessionToken creds."SessionToken")
          }

  toExternal :: InternalAssumeRoleResponse -> Aff DefaultClientProps
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
