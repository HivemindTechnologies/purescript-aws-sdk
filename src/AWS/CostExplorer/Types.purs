module AWS.CostExplorer.Types where

import Prelude (class Show)
import Data.Newtype (class Newtype)
import Simple.JSON (class WriteForeign, class ReadForeign)
import Data.Maybe (Maybe)
import Data.DateTime (DateTime)

newtype NextPageToken
  = NextPageToken String

derive instance ntNextPageToken :: Newtype NextPageToken _

derive newtype instance showNextPageToken :: Show NextPageToken

derive newtype instance wfNextPageToken :: WriteForeign NextPageToken

derive newtype instance rfNextPageToken :: ReadForeign NextPageToken

newtype Key
  = Key String

derive instance ntKey :: Newtype Key _

derive newtype instance showKey :: Show Key

derive newtype instance wfKey :: WriteForeign Key

derive newtype instance rfKey :: ReadForeign Key

newtype Amount
  = Amount String

derive instance ntAmount :: Newtype Amount _

derive newtype instance showAmount :: Show Amount

derive newtype instance wfAmount :: WriteForeign Amount

derive newtype instance rfAmount :: ReadForeign Amount

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
