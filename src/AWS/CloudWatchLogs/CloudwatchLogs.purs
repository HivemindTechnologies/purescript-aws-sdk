module AWS.CloudWatchLogs where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import Control.Monad.Error.Class (throwError)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn2, Fn3, Fn5, runFn2, runFn3, runFn5)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign (Foreign)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Simple.JSON (class WriteForeign, class ReadForeign, writeImpl)
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

newtype Destination
  = Destination String

newtype From
  = From Number

newtype To
  = To Number

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
  | NoRetention

instance writeForeignRetentionInDays :: WriteForeign RetentionInDays where
  writeImpl Retention1Day = writeImpl "1"
  writeImpl Retention3Days = writeImpl "3"
  writeImpl Retention5Days = writeImpl "5"
  writeImpl Retention1Week = writeImpl "7"
  writeImpl Retention2Weeks = writeImpl "14"
  writeImpl Retention1Month = writeImpl "30"
  writeImpl Retention2Months = writeImpl "60"
  writeImpl Retention3Months = writeImpl "90"
  writeImpl Retention4Months = writeImpl "120"
  writeImpl Retention5Months = writeImpl "150"
  writeImpl Retention6Months = writeImpl "180"
  writeImpl Retention12Months = writeImpl "365"
  writeImpl Retention13Months = writeImpl "400"
  writeImpl Retention18Months = writeImpl "545"
  writeImpl Retention24Months = writeImpl "731"
  writeImpl Retention60Months = writeImpl "1827"
  writeImpl Retention120Months = writeImpl "3653"
  writeImpl NoRetention = writeImpl "no retention"

retentionToInt :: RetentionInDays -> Maybe Int
retentionToInt retention = case retention of
  Retention1Day -> Just (1)
  Retention3Days -> Just (3)
  Retention5Days -> Just (5)
  Retention1Week -> Just (7)
  Retention2Weeks -> Just (14)
  Retention1Month -> Just (30)
  Retention2Months -> Just (60)
  Retention3Months -> Just (90)
  Retention4Months -> Just (120)
  Retention5Months -> Just (150)
  Retention6Months -> Just (180)
  Retention12Months -> Just (365)
  Retention13Months -> Just (400)
  Retention18Months -> Just (545)
  Retention24Months -> Just (731)
  Retention60Months -> Just (1827)
  Retention120Months -> Just (3653)
  NoRetention -> Nothing

toRetention :: Maybe Int -> RetentionInDays
toRetention retention = case retention of
  Just (1) -> Retention1Day
  Just (3) -> Retention3Days
  Just (5) -> Retention5Days
  Just (7) -> Retention1Week
  Just (14) -> Retention2Weeks
  Just (30) -> Retention1Month
  Just (60) -> Retention2Months
  Just (90) -> Retention3Months
  Just (120) -> Retention4Months
  Just (150) -> Retention5Months
  Just (180) -> Retention6Months
  Just (365) -> Retention12Months
  Just (400) -> Retention13Months
  Just (545) -> Retention18Months
  Just (731) -> Retention24Months
  Just (1827) -> Retention60Months
  Just (3653) -> Retention120Months
  Just (_) -> NoRetention
  Nothing -> NoRetention

type RetentionSettings
  = { logGroupName :: LogGroupName
    , retentionInDays :: RetentionInDays
    }

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

type ExportTaskParams
  = { destination :: Destination
    , from :: From
    , logGroupName :: LogGroupName
    , to :: To
    }

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

foreign import putRetentionPolicyImpl :: Fn3 CloudWatchLogs String (Nullable Int) (Effect (Promise Unit))

foreign import deleteRetentionPolicyImpl :: Fn2 CloudWatchLogs String (Effect (Promise Unit))

-- | Sets the retention policy for the log group.
-- | For setting NoRetention aka `Never Expire` use `deleteRetentionPolicy`. 
putRetentionPolicy :: CloudWatchLogs -> LogGroupName -> RetentionInDays -> Aff Unit
putRetentionPolicy cw (LogGroupName name) NoRetention = throwError $ error "Setting RetentionPolicy to NoRetention is not allowed. Use deleteRetentionPolicy instead."

putRetentionPolicy cw (LogGroupName name) retention =
  Promise.toAffE
    $ runFn3 putRetentionPolicyImpl cw name (toNullable $ retentionToInt $ retention)

-- | Deletes the retention policy from the log group, i.e. set's it to NoRetention aka `Never Expire`.
-- | For setting a retention policy use `putRetentionPolicy` instead.
deleteRetentionPolicy :: CloudWatchLogs -> LogGroupName -> Aff Unit
deleteRetentionPolicy cw (LogGroupName name) =
  Promise.toAffE
    $ runFn2 deleteRetentionPolicyImpl cw name

type CreateExportTaskResponse
  = { taskId :: String }

foreign import createExportTaskImpl :: Fn5 CloudWatchLogs String Number String Number (Effect (Promise CreateExportTaskResponse))

createExportTask :: CloudWatchLogs -> Destination -> From -> LogGroupName -> To -> Aff CreateExportTaskResponse
createExportTask cw (Destination destination) (From from) (LogGroupName logGroupName) (To to) =
  Promise.toAffE
    $ runFn5 createExportTaskImpl cw destination from logGroupName to
