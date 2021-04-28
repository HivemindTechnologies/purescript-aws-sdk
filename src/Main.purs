module Main where

import Prelude
import AWS.Core.Types (InstanceId(..), Region(..))
import AWS.EC2 (DescribeTagsResponse, Filter(..), ResourceTypeName(..))
import AWS.EC2 (describeInstanceTypeAttribute, describeTags)
import AWS.EC2 as EC2
import AWS.EC2.Types (Attribute(..))
import AWS.ECS (describeClusters, describeContainerInstances, describeTasks, listClusters, listContainerInstances, listTasks)
import AWS.ECS as ECS
import AWS.ECS.Types (ClusterArn(..), ContainerInstanceArn(..), TaskArn(..), Tasks)
import AWS.Pricing (getAllProducts, getProducts)
import AWS.Pricing as Pricing
import AWS.Pricing.Types (FilterField(..), FilterType(..), FilterValue(..), ServiceCode(..))
import Control.Promise (fromAff)
import Data.Argonaut (encodeJson)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow, log)

main :: Effect Unit
main = do
  ec2 <- EC2.newEC2 $ encodeJson {}
  ecs <- ECS.newECS $ encodeJson {}
  pricing <- Pricing.makeClient { region: Region "us-east-1" }
  _ <-
    fromAff do
      -- let 
      --   filters :: Array Filter
      --   filters = [ ResourceType [ Instance ] ]
      let
        filter1 =
          { type: TERM_MATCH
          , field: FilterField "ServiceCode"
          , value: FilterValue "AmazonEC2"
          }

        filter2 =
          { type: TERM_MATCH
          , field: FilterField "location"
          , value: FilterValue "EU (Frankfurt)"
          }
      -- res <- describeTags ec2 filters
      -- res <- describeTasks ecs [ TaskArn "arn:aws:ecs:eu-central-1:677840207937:task/cbis-production-microservices/7e66abc54a4b457399417f57d77a4582" ] (ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices")
      -- res <- listClusters ecs 
      -- res <- listTasks ecs (ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices") (ContainerInstanceArn "arn:aws:ecs:eu-central-1:677840207937:container-instance/cbis-production-microservices/59f156757059479f8c1a8a742174c15c")
      -- res <- listContainerInstances ecs (ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices")
      -- res <- describeClusters ecs [ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices"]
      -- res <- describeContainerInstances ecs (ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices")  [ContainerInstanceArn "arn:aws:ecs:eu-central-1:677840207937:container-instance/cbis-production-microservices/59f156757059479f8c1a8a742174c15c"]
      -- res <- getProducts pricing [ filter1, filter2 ] (ServiceCode "AmazonEC2") Nothing (Just 3.0)
      res <- getAllProducts pricing [ filter1, filter2 ] (ServiceCode "AmazonEC2") Nothing Nothing
      -- let 
      --   arr :: Array String
      --   arr = res.priceList
      --   bla :: String
      --   bla = "wefwe"
      _ <- log $ show res
      -- _ <- log "bla"
      -- _ <- case res of
      --   Left e -> log e
      --   Right r -> log $ show r
      logShow "Done fromAff"
  logShow "Done do"
