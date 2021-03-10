module AWS.CloudWatchLogs
  ( CloudWatchLogs
  , DescribeLogGroupsResponse
  , LogGroup
  , RetentionSettings
  , toRetention
  , LogGroupName(..)
  , ExportTaskParams
  , Destination
  , From
  , To
  , RetentionInDays(..)
  , deleteRetentionPolicy
  , putRetentionPolicy
  , retentionToInt
  , describeLogGroups
  , createExportTask
  , makeClient
  , RetentionInDays(..)
  ) where

import Prelude
import AWS.CloudWatch (makeClient)
import AWS.Core.Client (makeClientHelper)asda
import AWS.Core.Types (DefaultClientProps)
import Control.Monad.Error.Class (throwError)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Function.Uncurried (Fn2, Fn3, Fn5, runFn2, runFn3, runFn5)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))

foreign import data CloudWatchLogs :: Type

foreign import newCloudWatchLogs :: Json -> (Effect CloudWatchLogs)

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

derive newtype instance encodeLogGroupName :: EncodeJson LogGroupName

derive newtype instance decodeLogGroupName :: DecodeJson LogGroupName

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

instance encodeRetentionInDays :: EncodeJson RetentionInDays where
  encodeJson Retention1Day = encodeString "1"
  encodeJson Retention3Days = encodeString "3"
  encodeJson Retention5Days = encodeString "5"
  encodeJson Retention1Week = encodeString "7"
  encodeJson Retention2Weeks = encodeString "14"
  encodeJson Retention1Month = encodeString "30"
  encodeJson Retention2Months = encodeString "60"
  encodeJson Retention3Months = encodeString "90"
  encodeJson Retention4Months = encodeString "120"
  encodeJson Retention5Months = encodeString "150"
  encodeJson Retention6Months = encodeString "180"
  encodeJson Retention12Months = encodeString "365"
  encodeJson Retention13Months = encodeString "400"
  encodeJson Retention18Months = encodeString "545"
  encodeJson Retention24Months = encodeString "731"
  encodeJson Retention60Months = encodeString "1827"
  encodeJson Retention120Months = encodeString "3653"
  encodeJson NoRetention = encodeString "no retention"

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
  = { logGroups :: Array InternalLogGroup
    , nextToken :: Nullable String
    }

type DescribeLogGroupsResponse
  = { logGroups :: Array LogGroup
    , nextToken :: Maybe String
    }

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
  = { logStreams :: Array InternalLogStream
    , nextToken :: Nullable String
    }

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
  = { logStreams :: Array LogStream
    , nextToken :: Maybe String
    }

type InternalDescribeLogStreamsParams
  = { logGroupName :: String
    , nextToken :: Nullable String
    }

type DescribeLogStreamsParams
  = { logGroupName :: String
    , nextToken :: Maybe String
    }

type InternalDescribeLogGroupsParams
  = { nextToken :: Nullable String }

type DescribeLogGroupsParams
  = { nextToken :: Maybe String }

type ExportTaskParams
  = { destination :: Destination
    , from :: From
    , logGroupName :: LogGroupName
    , to :: To
    }

foreign import describeLogGroupsImpl :: Fn2 CloudWatchLogs InternalDescribeLogGroupsParams (Effect (Promise InternalDescribeLogGroupsResponse))

describeLogGroups ::
  CloudWatchLogs ->
  DescribeLogGroupsParams ->
  Aff DescribeLogGroupsResponse
describeLogGroups cloudWatchLogs params = liftEffect (curried cloudWatchLogs internalParams) >>= Promise.toAff <#> toExternal
  where
  toExternal :: InternalDescribeLogGroupsResponse -> DescribeLogGroupsResponse
  toExternal { logGroups: internal, nextToken } = { logGroups: internal <#> toExternalLogGroup, nextToken: Nullable.toMaybe nextToken }

  toExternalLogGroup :: InternalLogGroup -> LogGroup
  toExternalLogGroup internal =
    { creationTime: Nullable.toMaybe internal.creationTime
    , logGroupName: Nullable.toMaybe internal.logGroupName
    , retentionInDays: Nullable.toMaybe internal.retentionInDays
    , storedBytes: Nullable.toMaybe internal.storedBytes
    }

  internalParams = { nextToken: Nullable.toNullable params.nextToken }

  curried :: CloudWatchLogs -> InternalDescribeLogGroupsParams -> Effect (Promise InternalDescribeLogGroupsResponse)
  curried = runFn2 describeLogGroupsImpl

foreign import describeLogStreamsImpl :: Fn2 CloudWatchLogs InternalDescribeLogStreamsParams (Effect (Promise InternalDescribeLogStreamsResponse))

describeLogStreams ::
  CloudWatchLogs ->
  DescribeLogStreamsParams ->
  Aff DescribeLogStreamsResponse
describeLogStreams cloudWatchLogs params = liftEffect (curried cloudWatchLogs internalParams) >>= Promise.toAff <#> toExternal
  where
  toExternal :: InternalDescribeLogStreamsResponse -> DescribeLogStreamsResponse
  toExternal { logStreams: internal, nextToken } = { logStreams: internal <#> toExternalLogStream, nextToken: Nullable.toMaybe nextToken }

  toExternalLogStream :: InternalLogStream -> LogStream
  toExternalLogStream internal =
    { creationTime: Nullable.toMaybe internal.creationTime
    , firstEventTimestamp: Nullable.toMaybe internal.firstEventTimestamp
    , lastEventTimestamp: Nullable.toMaybe internal.lastEventTimestamp
    , lastIngestionTime: Nullable.toMaybe internal.lastIngestionTime
    }

  internalParams =
    { logGroupName: params.logGroupName
    , nextToken: Nullable.toNullable params.nextToken
    }

  curried :: CloudWatchLogs -> InternalDescribeLogStreamsParams -> Effect (Promise InternalDescribeLogStreamsResponse)
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
