module AWS.CloudWatch
  ( CloudWatch
  , makeClient
  , getMetricStatisticsAverage
  , getMetricStatisticsMaximum
  ) where

import Prelude
import AWS.CloudWatch.Types (Dimension, GetMetricStatisticsOutput, MetricDatapointMaximum, MetricSettings, Statistics, MetricDatapointAverage)
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import AWS.Core.Util (handleError)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (Json, parseJson)
import Data.Argonaut.Decode (decodeJson)
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

foreign import newCloudWatch :: Json -> (Effect CloudWatch)

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

type InternalDimension
  = { "Name" :: String, "Value" :: String }

toInternalDimension :: Dimension -> InternalDimension
toInternalDimension dimension =
  { "Name": show dimension.name
  , "Value": unwrap dimension.value
  }

toInternalGetMetricStatisticsParams ::
  forall a.
  { start :: DateTime, end :: DateTime | a } ->
  Statistics ->
  MetricSettings ->
  InternalGetMetricsStatisticsParams
toInternalGetMetricStatisticsParams range statistics ms =
  { "Namespace": nameSpace
  , "MetricName": metricName
  , "Dimensions": dimensions
  , "Statistics": stat
  , "StartTime": start
  , "EndTime": end
  , "Period": 3600 -- per hour
  }
  where
  nameSpace = show ms.nameSpace

  metricName = show ms.metricName

  dimensions = ms.dimensions <#> toInternalDimension

  stat = statistics <#> show

  start = fromDateTime range.start

  end = fromDateTime range.end

foreign import getMetricStatisticsImpl :: Fn2 CloudWatch InternalGetMetricsStatisticsParams (Effect (Promise String))

curried ::
  forall a.
  CloudWatch ->
  { start :: DateTime, end :: DateTime | a } ->
  Statistics ->
  MetricSettings ->
  Effect (Promise String)
curried cw range statistics ms = runFn2 getMetricStatisticsImpl cw params
  where
  params :: InternalGetMetricsStatisticsParams
  params = toInternalGetMetricStatisticsParams range statistics ms

getMetricStatisticsMaximum ::
  forall a.
  CloudWatch ->
  { start :: DateTime, end :: DateTime | a } ->
  Statistics ->
  MetricSettings ->
  Aff (Either String (GetMetricStatisticsOutput MetricDatapointMaximum))
getMetricStatisticsMaximum cw range statistics ms = liftEffect (curried cw range statistics ms) >>= Promise.toAff <#> parse
  where
  parse :: String -> Either String (GetMetricStatisticsOutput MetricDatapointMaximum)
  parse = (parseJson >=> decodeJson) >>> lmap handleError

getMetricStatisticsAverage ::
  forall a.
  CloudWatch ->
  { start :: DateTime, end :: DateTime | a } ->
  Statistics ->
  MetricSettings ->
  Aff (Either String (GetMetricStatisticsOutput MetricDatapointAverage))
getMetricStatisticsAverage cw range statistics ms = liftEffect (curried cw range statistics ms) >>= Promise.toAff <#> parse
  where
  parse :: String -> Either String (GetMetricStatisticsOutput MetricDatapointAverage)
  parse = (parseJson >=> decodeJson) >>> lmap handleError
