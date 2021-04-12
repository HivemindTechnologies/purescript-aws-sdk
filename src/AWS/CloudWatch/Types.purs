module AWS.CloudWatch.Types where

import Data.Newtype (class Newtype)
import Prelude (class Show)

data Statistic
  = Minimum
  | Maximum
  | Average
  | SampleCount
  | Sum

instance showStatistic :: Show Statistic where
  show Minimum = "Minimum"
  show Maximum = "Maximum"
  show Average = "Average"
  show SampleCount = "SampleCount"
  show Sum = "Sum"

type Statistics
  = Array Statistic

newtype DimensionValue
  = DimensionValue String

derive instance ntDimensionValue :: Newtype DimensionValue _

derive newtype instance showDimensionValue :: Show DimensionValue

data NameSpace
  = EC2
  | ECS

instance showNameSpace :: Show NameSpace where
  show EC2 = "AWS/EC2"
  show ECS = "AWS/ECS"

data DimensionName
  = EC2InstanceId
  | ClusterName

instance showDimensionName :: Show DimensionName where
  show EC2InstanceId = "InstanceId"
  show ClusterName = "ClusterName"

data MetricName
  = CPUUtilization
  | CPUReservation

instance showMetricName :: Show MetricName where
  show CPUUtilization = "CPUUtilization"
  show CPUReservation = "CPUReservation"

type Dimension
  = { name :: DimensionName, value :: DimensionValue }

type MetricSettings
  = { nameSpace :: NameSpace
    , metricName :: MetricName
    , dimensions :: Array Dimension
    }

type Raw
  = ( "Timestamp" :: String, "Unit" :: String )

type MetricDatapointAverage
  = { "Average" :: Number | Raw }

type MetricDatapointMaximum
  = { "Maximum" :: Number | Raw }

type GetMetricStatisticsOutput a
  = { "ResponseMetadata" :: { "RequestId" :: String }
    , "Label" :: String
    , "Datapoints" :: Array a
    }
