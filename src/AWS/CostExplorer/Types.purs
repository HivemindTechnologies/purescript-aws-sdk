module AWS.CostExplorer.Types where

import Prelude (class Show)
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe)
import Data.DateTime (DateTime)

newtype NextPageToken
  = NextPageToken String

derive instance ntNextPageToken :: Newtype NextPageToken _

derive newtype instance showNextPageToken :: Show NextPageToken

newtype Key
  = Key String

derive instance ntKey :: Newtype Key _

derive newtype instance showKey :: Show Key

newtype Amount
  = Amount String

derive instance ntAmount :: Newtype Amount _

derive newtype instance showAmount :: Show Amount


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
