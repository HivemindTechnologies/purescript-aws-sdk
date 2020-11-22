module AWS.CostExplorer where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import AWS.Core.Util (raiseEither, toIso8601Date)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.DateTime (DateTime)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, notNull, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))

foreign import data CE :: Type

foreign import newCE :: Foreign -> (Effect CE)

makeClient ::
  forall r via.
  Justifiable { | r } { | via } =>
  Fillable { | via } DefaultClientProps =>
  { | r } ->
  Effect CE
makeClient r = (justifillVia viaProxy r :: DefaultClientProps) # makeClientHelper newCE
  where
  viaProxy :: Proxy { | via }
  viaProxy = Proxy

-- https://github.com/aws/aws-sdk-js/blob/dabf8b11e6e0d61d4dc2ab62717b8735fb8b29e4/clients/costexplorer.d.ts#L649
type InternalGetCostAndUsageResponse
  = { "ResultsByTime" :: Nullable (Array InternalResultByTime)
    , "GroupDefinitions" :: Nullable (Array InternalGroupDefinition)
    , "NextPageToken" :: Nullable (String)
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
    , "NextPageToken" :: Nullable (String)
    }

getCostAndUsage ::
  forall a.
  CE ->
  { start :: DateTime, end :: DateTime | a } ->
  Aff InternalGetCostAndUsageResponse
getCostAndUsage ce range = do
  start <- raiseEither $ toIso8601Date range.start
  end <- raiseEither $ toIso8601Date range.end
  _getCostAndUsage ce start end Nothing

_getCostAndUsage ::
  CE ->
  String ->
  String ->
  Maybe InternalGetCostAndUsageResponse ->
  Aff InternalGetCostAndUsageResponse
_getCostAndUsage ce start end previousResult = do
  result <-
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
          -- , "NextPageToken": (maybe null (\r -> r."NextPageToken") previousResult)
          , "NextPageToken": (toNullable $ previousResult >>= \pr -> toMaybe pr."NextPageToken")
          }
  let
    mergedResult =
      { "ResultsByTime":
          notNull
            $ ( maybe
                  mempty
                  (\pr -> fromMaybe mempty (toMaybe pr."ResultsByTime"))
                  previousResult
              )
            <> (fromMaybe mempty (toMaybe result."ResultsByTime"))
      , "GroupDefinitions":
          notNull
            $ ( maybe
                  mempty
                  (\pr -> fromMaybe mempty (toMaybe pr."GroupDefinitions"))
                  previousResult
              )
            <> (fromMaybe mempty (toMaybe result."GroupDefinitions"))
      , "NextPageToken": result."NextPageToken"
      }
  case (toMaybe result."NextPageToken") of
    Just _ -> _getCostAndUsage ce start end (Just mergedResult)
    Nothing -> pure mergedResult
