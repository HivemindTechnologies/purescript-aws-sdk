module AWS.S3
  ( S3
  , BucketKey(..)
  , BucketName(..)
  , BucketPolicyParams
  , BucketPolicy(..)
  , createBucket
  , CreateBucketResponse
  , getObject
  , GetObjectParams
  , GetObjectResponse
  , makeClient
  , putBucketPolicy
  , GetSignedUrlParams
  , Operation(..)
  , SignedUrl(..)
  , getSignedUrl
  ) where

import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps, Region(..), Tags)
import AWS.Core.Util (raiseEither)
import Control.Bind ((>>=))
import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json, JsonDecodeError, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)
import Data.Int (round)
import Data.Newtype (class Newtype)
import Data.Show (show)
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Node.Buffer (Buffer)
import Prelude (class Show, Unit, (#), ($), (<#>), (>>>))
import Type.Proxy (Proxy(..))

foreign import data S3 :: Type

foreign import newS3 :: Json -> Effect S3

makeClient ::
  forall r via.
  Justifiable { | r } { | via } =>
  Fillable { | via } DefaultClientProps =>
  { | r } ->
  Effect S3
makeClient r = makeClientHelper newS3 props
  where
  viaProxy :: Proxy { | via }
  viaProxy = Proxy

  props :: DefaultClientProps
  props = justifillVia viaProxy r

newtype BucketName
  = BucketName String

derive instance ntBucketName :: Newtype BucketName _

derive newtype instance showBucketName :: Show BucketName

type InternalGetObjectParams
  = { "Bucket" :: String
    , "Key" :: String
    }

type InternalGetObjectResponse
  = { "Body" :: Buffer
    , "ContentLength" :: Int
    , "ContentEncoding" :: String
    , "ContentType" :: String
    , "Metadata" :: Json
    }

foreign import getObjectImpl :: Fn2 S3 InternalGetObjectParams (Effect (Promise InternalGetObjectResponse))

newtype BucketKey
  = BucketKey String

derive instance ntBucketKey :: Newtype BucketKey _

type GetObjectParams
  = { bucket :: BucketName
    , key :: BucketKey
    }

type GetObjectResponse
  = { body :: Buffer
    , contentLength :: Int
    , contentEncoding :: String
    , contentType :: String
    , tags :: Tags 
    }

getObject :: S3 -> GetObjectParams -> Aff GetObjectResponse
getObject client { bucket: BucketName name, key: BucketKey key } =
  runFn2 getObjectImpl client params
    # toAffE
    >>= (convert >>> lmap show >>> raiseEither)
  where
  params =
    { "Bucket": name
    , "Key": key
    }

  convert :: InternalGetObjectResponse -> Either JsonDecodeError GetObjectResponse
  convert internalResponse = parse internalResponse."Metadata" <#> addTags
    where 
      addTags tags = { body: internalResponse."Body"
          , contentLength: internalResponse."ContentLength"
          , contentEncoding: internalResponse."ContentEncoding"
          , contentType: internalResponse."ContentType"
          , tags : tags 
          }
      parse :: Json -> Either JsonDecodeError Tags
      parse = decodeJson 
    

type InternalGetSignedUrlParams
  = { "Bucket" :: String
    , "Key" :: String
    , "Expires" :: Int
    }

type GetSignedUrlParams
  = { bucket :: BucketName
    , key :: BucketKey
    , expires :: Seconds
    }

data Operation
  = GetObject
  | PutObject

newtype SignedUrl
  = SignedUrl String

derive instance ntSignedUrl :: Newtype SignedUrl _

derive newtype instance showSignedUrl :: Show SignedUrl

foreign import getSignedUrlImpl :: Fn3 S3 String InternalGetSignedUrlParams (Effect (Promise String))

getSignedUrl :: S3 -> Operation -> GetSignedUrlParams -> Aff SignedUrl
getSignedUrl s3 operation { bucket: BucketName bucketName, key: BucketKey bucketKey, expires: Seconds seconds } = runFn3 getSignedUrlImpl s3 (toString operation) params # toAffE <#> SignedUrl
  where
  toString GetObject = "getObject"

  toString PutObject = "putObject"

  params = { "Bucket": bucketName, "Key": bucketKey, "Expires": round seconds }

foreign import createBucketImpl :: Fn3 S3 String InternalBucketConfiguration (Effect (Promise InternalCreateBucketResponse))

type InternalBucketConfiguration
  = { "LocationConstraint" :: String }

type InternalCreateBucketResponse
  = { "Location" :: String }

type CreateBucketResponse
  = { location :: Region }

createBucket :: S3 -> BucketName -> Region -> Aff CreateBucketResponse
createBucket s3 (BucketName name) (Region region) =
  runFn3 createBucketImpl s3 name bucketConfig
    # toAffE
    <#> convert
  where
  bucketConfig :: InternalBucketConfiguration
  bucketConfig = { "LocationConstraint": region }

  convert :: InternalCreateBucketResponse -> CreateBucketResponse
  convert internalResponse = { location: Region internalResponse."Location" }

foreign import putBucketPolicyImpl :: Fn3 S3 String String (Effect (Promise Unit))

type BucketPolicyParams
  = { name :: BucketName
    , policy :: BucketPolicy
    }

newtype BucketPolicy
  = BucketPolicy String

derive instance ntBucketPolicy :: Newtype BucketPolicy _

derive newtype instance showBucketPolicy :: Show BucketPolicy

putBucketPolicy :: S3 -> BucketName -> BucketPolicy -> Aff Unit
putBucketPolicy s3 (BucketName name) (BucketPolicy policy) =
  toAffE
    $ runFn3 putBucketPolicyImpl s3 name policy
