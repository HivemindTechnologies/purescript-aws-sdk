module AWS.CostExplorer.Types where

import Prelude (class Show)
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe)
import Data.DateTime (DateTime)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Decode (class DecodeJson)

newtype NextPageToken
  = NextPageToken String

derive instance ntNextPageToken :: Newtype NextPageToken _

derive newtype instance showNextPageToken :: Show NextPageToken

derive newtype instance encodeNextPageToken :: EncodeJson NextPageToken

derive newtype instance decodeNextPageToken :: DecodeJson NextPageToken

newtype Key
  = Key String

derive instance ntKey :: Newtype Key _

derive newtype instance showKey :: Show Key

derive newtype instance encodeKey :: EncodeJson Key

derive newtype instance decodeKey :: DecodeJson Key


newtype Amount
  = Amount String

derive instance ntAmount :: Newtype Amount _

derive newtype instance showAmount :: Show Amount

derive newtype instance encodeAmount :: EncodeJson Amount

derive newtype instance decodeAmount :: DecodeJson Amount

type GroupDefinition
  = { key :: Maybe Key }

type DateInterval
  = { start :: DateTime, end :: DateTime }

type MetricValue
  = { amount :: Maybe Amount }

type Metric
  = { unblendedCost :: Maybe MetricValue }

type Group
  = { keys :: Array Key, metrics :: Maybe Metric }

type ResultByTime
  = { timePeriod :: Maybe DateInterval, groups :: Array Group }

type CostAndUsage
  = { resultsByTime :: Array ResultByTime
    , groupDefinitions :: Array GroupDefinition
    , nextPageToken :: Maybe NextPageToken
    }
