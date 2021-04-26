module Main where 

import Prelude

import AWS.Core.Types (InstanceId(..))
import AWS.EC2 (DescribeTagsResponse, Filter(..), ResourceTypeName(..))
import AWS.EC2 (describeInstanceTypeAttribute, describeTags)
import AWS.EC2 as EC2
import AWS.EC2.Types (Attribute(..))
import AWS.ECS (describeClusters, describeContainerInstances, listClusters, listContainerInstances, listTasks)
import AWS.ECS as ECS
import AWS.ECS.Types (ClusterArn(..), ContainerInstanceArn(..))
import Control.Promise (fromAff)
import Data.Argonaut (encodeJson)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (logShow, log)

main :: Effect Unit
main = do 
  ec2 <- EC2.newEC2 $ encodeJson {}
  ecs <- ECS.newECS $ encodeJson {}
  _ <- fromAff do
    -- let 
    --   filters :: Array Filter
    --   filters = [ ResourceType [ Instance ] ]
    -- res <- describeTags ec2 filters
    res <- describeInstanceTypeAttribute ec2 InstanceType (InstanceId "i-0b7ae30b85f749328")
    -- res <- listClusters ecs 
    -- res <- listTasks ecs (ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices") (ContainerInstanceArn "arn:aws:ecs:eu-central-1:677840207937:container-instance/cbis-production-microservices/59f156757059479f8c1a8a742174c15c")
    -- res <- listContainerInstances ecs (ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices")
    -- res <- describeClusters ecs [ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices"]
    -- res <- describeContainerInstances ecs (ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices")  [ContainerInstanceArn "arn:aws:ecs:eu-central-1:677840207937:container-instance/cbis-production-microservices/59f156757059479f8c1a8a742174c15c"]
    _ <- case res of 
      Left e -> log e
      Right r ->  log $ show r
    logShow "Done fromAff"
  logShow "Done do"
