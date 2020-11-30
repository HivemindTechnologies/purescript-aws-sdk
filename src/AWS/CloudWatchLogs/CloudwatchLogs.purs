module AWS.CloudWatchLogs where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))

foreign import data CloudWatchLogs :: Type

foreign import newCloudWatchLogs :: Foreign -> (Effect CloudWatchLogs)

makeClient ::
  forall r via.
  Justifiable { | r } { | via } =>
  Fillable { | via } DefaultClientProps =>
  { | r } ->
  Effect CloudWatchLogs
makeClient r = makeClientHelper newCloudWatchLogs props
  where
  viaProxy :: Proxy { | via }
  viaProxy = Proxy

  props :: DefaultClientProps
  props = justifillVia viaProxy r

newtype LogGroupName
  = LogGroupName String

data RetentionInDays
  = Retention1
  | Retention3
  | Retention5
  | Retention7
  | Retention14
  | Retention30
  | Retention60
  | Retention90
  | Retention120
  | Retention150
  | Retention180
  | Retention365
  | Retention400
  | Retention545
  | Retention731
  | Retention1827

toRetentionInDaysInt :: RetentionInDays -> Int
toRetentionInDaysInt retention = case retention of
  Retention1 -> 1
  Retention3 -> 3
  Retention5 -> 5
  Retention7 -> 7
  Retention14 -> 14
  Retention30 -> 30
  Retention60 -> 60
  Retention90 -> 90
  Retention120 -> 120
  Retention150 -> 150
  Retention180 -> 180
  Retention365 -> 365
  Retention400 -> 400
  Retention545 -> 545
  Retention731 -> 731
  Retention1827 -> 1827

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

foreign import putRetentionPolicyImpl :: Fn3 CloudWatchLogs String Int (Effect (Promise Unit))

putRetentionPolicy :: CloudWatchLogs -> LogGroupName -> RetentionInDays -> Aff Unit
putRetentionPolicy cw (LogGroupName name) retention =
  Promise.toAffE
    $ runFn3 putRetentionPolicyImpl cw name (toRetentionInDaysInt $ retention)
