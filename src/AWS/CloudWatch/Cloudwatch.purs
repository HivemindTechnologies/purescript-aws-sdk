module AWS.CloudWatch where

import Prelude

import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps, InstanceId)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (JsonDecodeError, decodeJson, parseJson)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.JSDate (JSDate, fromDateTime)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))

foreign import data CloudWatch :: Type

foreign import newCloudWatch :: forall p. p -> (Effect CloudWatch)

makeClient ::
  forall r via.
  Justifiable { | r } { | via } =>
  Fillable { | via } DefaultClientProps =>
  { | r } ->
  Effect CloudWatch
makeClient r = makeClientHelper newCloudWatch props
  where
  viaProxy :: Proxy { | via }
  viaProxy = Proxy

  props :: DefaultClientProps
  props = justifillVia viaProxy r

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

foreign import getMetricStatisticsImpl :: Fn2 CloudWatch InternalGetMetricsStatisticsParams (Effect (Promise String))

getMetricStatistics :: forall a. CloudWatch -> { start :: DateTime, end :: DateTime | a } -> InstanceId -> Aff (Either String InternalGetMetricStatisticsOutput)
getMetricStatistics cw range instanceId = liftEffect curried >>= Promise.toAff <#> parse
  where
  handleError :: JsonDecodeError -> String
  handleError = show

  parse :: String -> Either String InternalGetMetricStatisticsOutput
  parse = (parseJson >=> decodeJson) >>> lmap handleError

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
