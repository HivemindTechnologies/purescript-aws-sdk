module AWS.Core.Types where

import Prelude
import Data.Newtype (class Newtype)
import Simple.JSON (class WriteForeign)
import Data.Maybe (Maybe)

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

type BasicClientPropsR r 
  = ( accessKeyId :: Maybe AccessKeyId
    , secretAccessKey :: Maybe SecretAccessKey
    , region :: Maybe Region
    , sessionToken :: Maybe SessionToken
    | r 
    )

type DefaultClientPropsR = BasicClientPropsR ()
type DefaultClientProps = Record DefaultClientPropsR
