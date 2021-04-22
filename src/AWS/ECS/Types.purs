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

newtype TaskArn
  = TaskArn String

derive instance ntTaskArn :: Newtype TaskArn _

derive newtype instance showTaskArn :: Show TaskArn

type Tasks
  = Array TaskArn

type ListTasksResponse
  = { taskArns :: Tasks }

newtype ContainerInstanceArn
  = ContainerInstanceArn String

derive instance ntContainerInstanceArn :: Newtype ContainerInstanceArn _

derive newtype instance showContainerInstanceArn :: Show ContainerInstanceArn

type ContainterInstances
  = Array ContainerInstanceArn

type ListContainerInstancesResponse
  = { containerInstanceArns :: ContainterInstances }
