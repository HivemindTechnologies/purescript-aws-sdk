module AWS.S3
  ( S3
  , makeClient
  , GetObjectParams
  , GetObjectResponse
  , getObject
  ) where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import Control.Promise (Promise, toAffE)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Node.Buffer (Buffer)
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
