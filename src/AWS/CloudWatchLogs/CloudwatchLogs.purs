module AWS.CloudWatchLogs where

import Prelude


import AWS.Core.Client (makeClientHelper, makeDefaultClient)
import AWS.Core.Types (DefaultClientPropsR, DefaultClientProps)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)

foreign import data CloudWatchLogs :: Type

foreign import newCloudWatchLogs :: Foreign -> (Effect CloudWatchLogs)

makeClient :: forall t4 t5 t6 t7 t8.
  RowToList t6 t5 => FillableFields t5 () t6 => Union t8 t6
                                                  DefaultClientPropsR
                                                 => RowToList t7 t4 => JustifiableFields t4 t7 () t8 => Record t7 -> Effect CloudWatchLogs
makeClient r = ((makeDefaultClient r:: DefaultClientProps)) # makeClientHelper newCloudWatchLogs

newtype LogGroupName
  = LogGroupName String
  
type InternalDescribeLogGroupsResponse
  = { logGroups :: Array InternalLogGroup }

type DescribeLogGroupsResponse
  = { logGroups :: Array LogGroup }

type InternalLogGroup
  = { arn :: Nullable String
    , creationTime :: Nullable Number
    , kmsKeyId :: Nullable String
    , logGroupName :: Nullable String
    , metricFilterCount :: Nullable Number
    , retentionInDays :: Nullable Number
    , storedBytes :: Nullable Number
    }

-- todo: Number to TimeStamp
type LogGroup
  = { creationTime :: Maybe Number
    , logGroupName :: Maybe String
    , retentionInDays :: Maybe Number
    , storedBytes :: Maybe Number
    }

type InternalDescribeLogStreamsResponse
  = { logStreams :: Array InternalLogStream }

type InternalLogStream
  = { creationTime :: Nullable Number
    , firstEventTimestamp :: Nullable Number
    , lastEventTimestamp :: Nullable Number
    , lastIngestionTime :: Nullable Number
    }

-- todo: Number to TimeStamp
type LogStream
  = { creationTime :: Maybe Number
    , firstEventTimestamp :: Maybe Number
    , lastEventTimestamp :: Maybe Number
    , lastIngestionTime :: Maybe Number
    }

type DescribeLogStreamsResponse
  = { logStreams :: Array LogStream }

foreign import describeLogGroupsImpl :: CloudWatchLogs -> (Effect (Promise InternalDescribeLogGroupsResponse))

describeLogGroups :: CloudWatchLogs -> Aff DescribeLogGroupsResponse
describeLogGroups cloudWatchLogs = liftEffect curried >>= Promise.toAff <#> toExternal
  where
  toExternal :: InternalDescribeLogGroupsResponse -> DescribeLogGroupsResponse
  toExternal { logGroups: internal } = { logGroups: internal <#> toExternalLogGroup }

  toExternalLogGroup :: InternalLogGroup -> LogGroup
  toExternalLogGroup internal =
    { creationTime: Nullable.toMaybe internal.creationTime
    , logGroupName: Nullable.toMaybe internal.logGroupName
    , retentionInDays: Nullable.toMaybe internal.retentionInDays
    , storedBytes: Nullable.toMaybe internal.storedBytes
    }

  curried :: Effect (Promise InternalDescribeLogGroupsResponse)
  curried = describeLogGroupsImpl cloudWatchLogs

foreign import describeLogStreamsImpl :: Fn2 CloudWatchLogs String (Effect (Promise InternalDescribeLogStreamsResponse))

describeLogStreams :: CloudWatchLogs -> LogGroupName -> Aff (DescribeLogStreamsResponse)
describeLogStreams cloudWatchLogs (LogGroupName name) = liftEffect (curried cloudWatchLogs name) >>= Promise.toAff <#> toExternal
  where
  toExternal :: InternalDescribeLogStreamsResponse -> DescribeLogStreamsResponse
  toExternal { logStreams: internal } = { logStreams: internal <#> toExternalLogStream }

  toExternalLogStream :: InternalLogStream -> LogStream
  toExternalLogStream internal =
    { creationTime: Nullable.toMaybe internal.creationTime
    , firstEventTimestamp: Nullable.toMaybe internal.firstEventTimestamp
    , lastEventTimestamp: Nullable.toMaybe internal.lastEventTimestamp
    , lastIngestionTime: Nullable.toMaybe internal.lastIngestionTime
    }

  curried :: CloudWatchLogs -> String -> Effect (Promise InternalDescribeLogStreamsResponse)
  curried = (runFn2 describeLogStreamsImpl)
