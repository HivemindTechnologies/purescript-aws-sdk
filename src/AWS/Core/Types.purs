module AWS.Core.Types where

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Object as F
import Prelude (class Show, bind, ($), pure, (>>>), map)

newtype AccessKeyId
  = AccessKeyId String

derive instance ntAccessKeyId :: Newtype AccessKeyId _

derive newtype instance encodeAccessKeyId :: EncodeJson AccessKeyId

newtype Region
  = Region String

derive instance ntRegion :: Newtype Region _

derive newtype instance encodeRegion :: EncodeJson Region

newtype SecretAccessKey
  = SecretAccessKey String

derive instance ntSecretAccessKey :: Newtype SecretAccessKey _

derive newtype instance encodeSecretAccessKey :: EncodeJson SecretAccessKey

newtype SessionToken
  = SessionToken String

derive instance ntSessionToken :: Newtype SessionToken _

derive newtype instance encodeSessionToken :: EncodeJson SessionToken

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

type Credentials
  = { accessKeyId :: Maybe AccessKeyId
    , secretAccessKey :: Maybe SecretAccessKey
    , sessionToken :: Maybe SessionToken
    }

newtype Endpoint
  = Endpoint String

derive instance ntEndpoint :: Newtype Endpoint _

derive newtype instance showEndpoint :: Show Endpoint

derive newtype instance encodeEndpoint :: EncodeJson Endpoint

decodeAsMap :: forall r. DecodeJson r => Json -> Either JsonDecodeError (Map.Map String r)
decodeAsMap str = do
  obj <- decodeJson str
  pure $ Map.fromFoldable $ (F.toUnfoldable obj :: Array _)

newtype Tags
  = Tags (Map.Map String String)

derive newtype instance showTags :: Show Tags

derive instance ntTags :: Newtype Tags _

instance tagsDecoder :: DecodeJson Tags where
  decodeJson = decodeAsMap >>> map Tags

type BasicClientPropsR r
  = ( accessKeyId :: Maybe AccessKeyId
    , secretAccessKey :: Maybe SecretAccessKey
    , region :: Maybe Region
    , endpoint :: Maybe Endpoint
    , sessionToken :: Maybe SessionToken
    , credentials :: Maybe Credentials
    | r
    )

type BasicClientProps r
  = Record (BasicClientPropsR r)

type DefaultClientPropsR
  = BasicClientPropsR ()

type DefaultClientProps
  = Record DefaultClientPropsR
