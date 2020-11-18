module AWS.CloudWatch where

import Prelude


import AWS.Core.Client (makeClientHelper, makeDefaultClient)
import AWS.Core.Types (DefaultClientProps, DefaultClientPropsR, InstanceId)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.JSDate (JSDate, fromDateTime)
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign, ForeignError)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Simple.JSON (readJSON)

foreign import data CloudWatch :: Type

foreign import newCloudWatch :: Foreign -> (Effect CloudWatch)

makeClient :: forall t4 t5 t6 t7 t8.
  RowToList t6 t5 => FillableFields t5 () t6 => Union t8 t6
                                                  DefaultClientPropsR
                                                 => RowToList t7 t4 => JustifiableFields t4 t7 () t8 => Record t7 -> Effect CloudWatch
makeClient r = ((makeDefaultClient r:: DefaultClientProps)) # makeClientHelper newCloudWatch

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
  handleError :: NonEmptyList ForeignError -> String
  handleError = map show >>> fold

  parse :: String -> Either String InternalGetMetricStatisticsOutput
  parse = readJSON >>> lmap handleError

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
