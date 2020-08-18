module AWS.Cloudwatch where

import Prelude
import AWS.Core (AccessKeyId(..), InstanceId, Region(..), SecretAccessKey(..), SessionToken(..))
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Function.Uncurried (Fn1, Fn2, runFn2)
import Data.JSDate (JSDate, fromDateTime)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Newtype (un, unwrap)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (ForeignError)
import Simple.JSON (readJSON)

foreign import data Cloudwatch :: Type

type InternalMakeClientParams
  = { region :: String
    , secretAccessKey :: String
    , accessKeyId :: String
    , sessionToken :: Nullable String
    }

foreign import makeClientImpl :: Fn1 InternalMakeClientParams (Effect Cloudwatch)

makeClient :: Region -> AccessKeyId -> SecretAccessKey -> Maybe SessionToken -> Effect Cloudwatch
makeClient r a s t =
  makeClientImpl
    { region: (un Region r)
    , secretAccessKey: (un SecretAccessKey s)
    , accessKeyId: (un AccessKeyId a)
    , sessionToken: toNullable $ map (un SessionToken) t
    }

type InternalGetMetricsStatisticsParams
  = { "Dimensions" :: Array { "Name" :: String, "Value" :: String }
    , "MetricName" :: String
    , "Namespace" :: String
    , "Period" :: Int
    , "StartTime" :: JSDate
    , "EndTime" :: JSDate
    , "Statistics" :: Array String
    }

type InternalMetricDatapoint
  = { "Timestamp" :: String
    , "Maximum" :: Number
    , "Unit" :: String
    }

type InternalGetMetricStatisticsOutput
  = { "ResponseMetadata" :: { "RequestId" :: String }
    , "Label" :: String
    , "Datapoints" :: Array InternalMetricDatapoint
    }

foreign import data GetMetricStatisticsResponse :: Type

foreign import getMetricStatisticsImpl :: Fn2 Cloudwatch InternalGetMetricsStatisticsParams (Effect (Promise String))

getMetricStatistics :: forall a. Cloudwatch -> { start :: DateTime, end :: DateTime | a } -> InstanceId -> Aff (Either String InternalGetMetricStatisticsOutput)
getMetricStatistics cw range instanceId = liftEffect curried >>= Promise.toAff <#> parse
  where
  handleError :: NonEmptyList ForeignError -> String
  handleError = map show >>> fold

  parse :: String -> Either String InternalGetMetricStatisticsOutput
  parse = readJSON >>> bimap handleError identity

  curried :: Effect (Promise String)
  curried =
    runFn2 getMetricStatisticsImpl cw
      { "Namespace": "AWS/EC2"
      , "MetricName": "CPUUtilization"
      , "Dimensions": [ { "Name": "InstanceId", "Value": unwrap instanceId } ]
      , "Statistics": [ "Maximum" ]
      , "StartTime": fromDateTime range.start
      , "EndTime": fromDateTime range.end
      , "Period": 120000
      }
