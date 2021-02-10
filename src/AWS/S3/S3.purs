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
  ) where

import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps, Region(..))
import Control.Promise (Promise, toAffE)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Node.Buffer (Buffer)
import Prelude (Unit, ($), class Show, (#), (<#>))
import Type.Proxy (Proxy(..))

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
    , contentLenght :: Int
    , contentEncoding :: String
    , contentType :: String
    }

getObject :: S3 -> GetObjectParams -> Aff GetObjectResponse
getObject client { bucket: BucketName name, key: BucketKey key } =
  runFn2 getObjectImpl client params
    # toAffE
    <#> convert
  where
  params =
    { "Bucket": name
    , "Key": key
    }

  convert :: InternalGetObjectResponse -> GetObjectResponse
  convert internalResponse =
    { body: internalResponse."Body"
    , contentLenght: internalResponse."ContentLength"
    , contentEncoding: internalResponse."ContentEncoding"
    , contentType: internalResponse."ContentType"
    }

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
