module AWS.CloudWatchLogs where

import Prelude (class Show)
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
import Data.Newtype (class Newtype)
import Simple.JSON (class WriteForeign, class ReadForeign)

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

derive instance ntLogGroupName :: Newtype LogGroupName _

derive newtype instance showLogGroupName :: Show LogGroupName

derive newtype instance wfLogGroupName :: WriteForeign LogGroupName

derive newtype instance rfLogGroupName :: ReadForeign LogGroupName

data RetentionInDays
  = Retention1Day
  | Retention3Days
  | Retention5Days
  | Retention1Week
  | Retention2Weeks
  | Retention1Month
  | Retention2Months
  | Retention3Months
  | Retention4Months
  | Retention5Months
  | Retention6Months
  | Retention12Months
  | Retention13Months
  | Retention18Months
  | Retention24Months
  | Retention60Months
  | Retention120Months

toRetentionInDaysInt :: RetentionInDays -> Int
toRetentionInDaysInt retention = case retention of
  Retention1Day -> 1
  Retention3Days -> 3
  Retention5Days -> 5
  Retention1Week -> 7
  Retention2Weeks -> 14
  Retention1Month -> 30
  Retention2Months -> 60
  Retention3Months -> 90
  Retention4Months -> 120
  Retention5Months -> 150
  Retention6Months -> 180
  Retention12Months -> 365
  Retention13Months -> 400
  Retention18Months -> 545
  Retention24Months -> 731
  Retention60Months -> 1827
  Retention120Months -> 3653

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
