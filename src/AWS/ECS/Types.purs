module AWS.ECS.Types where

import Prelude (class Show)
import Data.Newtype (class Newtype)

newtype ClusterArn
  = ClusterArn String

derive instance ntClusterArn :: Newtype ClusterArn _

derive newtype instance showClusterArn :: Show ClusterArn

type Clusters
  = Array ClusterArn

type ListClustersResponse
  = { clusterArns :: Clusters }
