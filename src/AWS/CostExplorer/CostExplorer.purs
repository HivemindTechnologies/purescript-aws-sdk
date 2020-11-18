module AWS.CostExplorer where

import Prelude

import AWS.Core.Client (makeClientHelper, makeDefaultClient)
import AWS.Core.Types (PropsDefaultR)
import AWS.Core.Util (raiseEither, toIso8601Date)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.DateTime (DateTime)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)

foreign import data CE :: Type

foreign import newCE :: Foreign -> (Effect CE)

type PropsR = PropsDefaultR ()
type Props = Record PropsR

makeClient :: forall t4 t5 t6 t7 t8.
  RowToList t6 t5 => FillableFields t5 () t6 => Union t8 t6
                                                  PropsR
                                                 => RowToList t7 t4 => JustifiableFields t4 t7 () t8 => Record t7 -> Effect CE
makeClient r = ((makeDefaultClient r:: Props)) # makeClientHelper newCE

-- https://github.com/aws/aws-sdk-js/blob/dabf8b11e6e0d61d4dc2ab62717b8735fb8b29e4/clients/costexplorer.d.ts#L649
type InternalGetCostAndUsageResponse
  = { "ResultsByTime" :: Nullable (Array InternalResultByTime)
    , "GroupDefinitions" :: Nullable (Array InternalGroupDefinition)
    }

type InternalGroupDefinition
  = { "Key" :: Nullable String }

type InternalResultByTime
  = { "TimePeriod" :: Nullable InternalDateInterval, "Groups" :: Nullable (Array InternalGroup) }

type InternalDateInterval
  = { "Start" :: String }

type InternalGroup
  = { "Keys" :: Nullable (Array String), "Metrics" :: Nullable InternalMetric }

type InternalMetric
  = { "UnblendedCost" :: Nullable InternalMetricValue }

type InternalMetricValue
  = { "Amount" :: Nullable String }

foreign import getCostAndUsageImpl :: Fn2 CE InternalGetCostAndUsageParams (Effect (Promise InternalGetCostAndUsageResponse))

-- https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/CostExplorer.html#getCostAndUsage-property
type InternalGetCostAndUsageParams
  = { "Granularity" :: String
    , "GroupBy" :: Array { "Key" :: String, "Type" :: String }
    , "Metrics" :: Array String
    , "TimePeriod" :: { "End" :: String, "Start" :: String }
    }

getCostAndUsage :: forall a. CE -> { start :: DateTime, end :: DateTime | a } -> Aff InternalGetCostAndUsageResponse
getCostAndUsage ce range = do
  start <- raiseEither $ toIso8601Date range.start
  end <- raiseEither $ toIso8601Date range.end
  Promise.toAffE
    $ runFn2 getCostAndUsageImpl ce
        { "TimePeriod":
            { "Start": start
            , "End": end
            }
        , "Granularity": "DAILY"
        , "GroupBy":
            [ { "Key": "SERVICE"
              , "Type": "DIMENSION"
              }
            , { "Key": "USAGE_TYPE"
              , "Type": "DIMENSION"
              }
            ]
        , "Metrics": [ "UnblendedCost" ]
        }
