module AWS.Lambda where

import Prelude

import AWS.Core (Arn(..), Region(..), AccessKeyId(..), SecretAccessKey(..))
import Control.Promise (Promise, toAffE)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff)
import Simple.JSON (class WriteForeign, writeJSON)

type InternalLambdaParams = {
    "FunctionName" :: String,
    "Payload" :: String
}

foreign import data Lambda :: Type

type InternalMakeClientParams
  = { region :: String
    , secretAccessKey :: String
    , accessKeyId :: String
    }

foreign import makeClientImpl :: InternalMakeClientParams -> (Effect Lambda)

foreign import makeDefaultClientImpl :: Effect Lambda

makeClient :: Region -> AccessKeyId -> SecretAccessKey -> Effect Lambda
makeClient r a s =
  makeClientImpl
    { region: un Region r
    , secretAccessKey: un SecretAccessKey s
    , accessKeyId: un AccessKeyId a
    }

makeDefaultClient :: Effect Lambda
makeDefaultClient = makeDefaultClientImpl

foreign import invokeFunctionImpl :: forall output. Fn2 Lambda InternalLambdaParams (Effect (Promise output))

invokeFunction :: forall input output. WriteForeign input => Lambda -> Arn -> input -> Aff output
invokeFunction client (Arn arn) input = runFn2 invokeFunctionImpl client params # toAffE
    where
      params = {
          "FunctionName" : arn,
          "Payload" : writeJSON input
      }
