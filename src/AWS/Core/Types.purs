module AWS.Core.Types where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign (Foreign, tagOf)
import Prelude (class Show)
import Simple.JSON (class WriteForeign)

newtype AccessKeyId
  = AccessKeyId String

derive instance ntAccessKeyId :: Newtype AccessKeyId _

derive newtype instance wfAccessKeyId :: WriteForeign AccessKeyId

newtype Region
  = Region String

derive instance ntRegion :: Newtype Region _

derive newtype instance wfRegion :: WriteForeign Region

newtype SecretAccessKey
  = SecretAccessKey String

derive instance ntSecretAccessKey :: Newtype SecretAccessKey _

derive newtype instance wfSecretAccessKey :: WriteForeign SecretAccessKey

newtype SessionToken
  = SessionToken String

derive instance ntSessionToken :: Newtype SessionToken _

derive newtype instance wfSessionToken :: WriteForeign SessionToken

newtype Arn
  = Arn String

derive instance ntArn :: Newtype Arn _

newtype ExternalId
  = ExternalId String

derive instance ntExternalId :: Newtype ExternalId _

type Instance
  = { id :: InstanceId, "type" :: InstanceType }

newtype InstanceId
  = InstanceId String

derive instance ntInstanceId :: Newtype InstanceId _

derive newtype instance showInstanceId :: Show InstanceId

newtype InstanceType
  = InstanceType String

derive instance ntInstanceType :: Newtype InstanceType _

derive newtype instance showInstanceType :: Show InstanceType

newtype Credentials
  = Credentials Foreign

derive instance ntCredentials :: Newtype Credentials _

instance showCredentials :: Show Credentials where
  show (Credentials credentials) = tagOf credentials

instance writeCreds :: WriteForeign Credentials where
  writeImpl (Credentials creds) = creds

type BasicClientPropsR r
  = ( accessKeyId :: Maybe AccessKeyId
    , secretAccessKey :: Maybe SecretAccessKey
    , region :: Maybe Region
    , sessionToken :: Maybe SessionToken
    , credentials :: (Maybe Credentials)
    | r
    )

type DefaultClientPropsR
  = BasicClientPropsR ()

type DefaultClientProps
  = Record DefaultClientPropsR
