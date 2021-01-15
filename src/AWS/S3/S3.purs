module AWS.S3 where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import Control.Promise (Promise, toAffE)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Node.Buffer (Buffer)
import Type.Proxy (Proxy(..))
import AWS.Core.Types (DefaultClientProps, Region(..))

foreign import data S3 :: Type

foreign import newS3 :: Foreign -> Effect S3

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

type InternalGetObjectParams
  = { "Bucket" :: String
    , "Key" :: String
    }

type InternalGetObjectResponse
  = { "Body" :: Buffer
    , "ContentLength" :: Int
    , "ContentEncoding" :: String
    , "ContentType" :: String
    }

foreign import getObjectImpl :: Fn2 S3 InternalGetObjectParams (Effect (Promise InternalGetObjectResponse))

type GetObjectParams
  = { bucket :: String
    , key :: String
    }

type GetObjectResponse
  = { body :: Buffer
    , contentLenght :: Int
    , contentEncoding :: String
    , contentType :: String
    }

getObject :: S3 -> GetObjectParams -> Aff GetObjectResponse
getObject client input = runFn2 getObjectImpl client params # toAffE <#> convert
  where
  params =
    { "Bucket": input.bucket
    , "Key": input.key
    }

  convert :: InternalGetObjectResponse -> GetObjectResponse
  convert internalResponse =
    { body: internalResponse."Body"
    , contentLenght: internalResponse."ContentLength"
    , contentEncoding: internalResponse."ContentEncoding"
    , contentType: internalResponse."ContentType"
    }

foreign import createBucketImpl :: Fn3 S3 String InternalBucketConfiguration (Effect (Promise Unit))

type InternalBucketConfiguration
  = { "LocationConstraint" :: String }

newtype BucketName
  = BucketName String

createBucket :: S3 -> BucketName -> Region -> Aff Unit
createBucket s3 (BucketName name) (Region region) =
  toAffE
    $ runFn3 createBucketImpl s3 name bucketConfig
  where
  bucketConfig :: InternalBucketConfiguration
  bucketConfig = { "LocationConstraint": region }
