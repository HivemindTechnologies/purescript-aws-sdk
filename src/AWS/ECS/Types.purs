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

type ContainerInstances
  = Array ContainerInstanceArn

type ListContainerInstancesResponse
  = { containerInstanceArns :: ContainerInstances }

type ClusterParams
  = { clusterArn :: String
    , clusterName :: String
    , status :: String
    , registeredContainerInstancesCount :: Number
    }

type DescribeClustersResponse
  = { clusters :: Array ClusterParams }

type ContainerInstanceParams
  = { containerInstanceArn :: String
    , ec2InstanceId :: String
    }

type DescribeContainerInstancesResponse
  = { containerInstances :: Array ContainerInstanceParams }

{-
{
  tasks: [
    {
      attachments: [],
      availabilityZone: 'eu-central-1b',
      clusterArn: 'arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices',
      connectivity: 'CONNECTED',
      connectivityAt: 2021-01-18T16:43:32.008Z,
      containerInstanceArn: 'arn:aws:ecs:eu-central-1:677840207937:container-instance/cbis-production-microservices/59f156757059479f8c1a8a742174c15c',
      containers: [Array],
      cpu: '512',
      createdAt: 2021-01-18T16:43:32.008Z,
      desiredStatus: 'RUNNING',
      enableExecuteCommand: false,
      group: 'service:app-configuration',
      healthStatus: 'UNKNOWN',
      lastStatus: 'RUNNING',
      launchType: 'EC2',
      memory: '512',
      overrides: [Object],
      pullStartedAt: 2021-01-18T16:43:32.115Z,
      pullStoppedAt: 2021-01-18T16:44:01.115Z,
      startedAt: 2021-01-18T16:44:02.115Z,
      startedBy: 'ecs-svc/7390572488766464431',
      tags: [],
      taskArn: 'arn:aws:ecs:eu-central-1:677840207937:task/cbis-production-microservices/7e66abc54a4b457399417f57d77a4582',
      taskDefinitionArn: 'arn:aws:ecs:eu-central-1:677840207937:task-definition/cbis-app-configuration-production-app-configuration:2',
      version: 2
    }
  ],
  failures: []
}
-}
type TaskDetails r
  = { cpu :: String
    , memory :: String
    | r
    }

type Failure
  = { arn :: String
    , reason :: String
    , detail :: String
    }

type DescribeTasksResponse r
  = { tasks :: Array (TaskDetails r)
    , failures :: Array Failure
    }
