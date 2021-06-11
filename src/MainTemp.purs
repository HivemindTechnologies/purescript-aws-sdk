module MainTemp where

import Prelude

import AWS.Core.Types (InstanceId(..), Region(..))
import AWS.EC2 (DescribeTagsResponse, Filter(..), ResourceTypeName(..))
import AWS.EC2 (describeInstanceTypeAttribute, describeTags)
import AWS.EC2 as EC2
import AWS.EC2.Types (Attribute(..))
import AWS.ECS (describeClusters, describeContainerInstances, describeTasks, listClusters, listContainerInstances, listTasks)
import AWS.ECS as ECS
import AWS.ECS.Types (ClusterArn(..), ContainerInstanceArn(..), TaskArn(..), Tasks)
import AWS.Pricing (getAllEC2Products, getEC2Products, getECSProducts)
import AWS.Pricing as Pricing
import AWS.Pricing.ECS.Types (ECSUsageType(..))
import AWS.Pricing.Types (FilterField(..), FilterType(..), FilterValue(..), ServiceCode(..))
import Control.Promise (fromAff)
import Data.Argonaut (encodeJson)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow, log)

mainTemp :: Effect Unit
mainTemp = do
  ec2 <- EC2.newEC2 $ encodeJson {}
  ecs <- ECS.newECS $ encodeJson {}
  pricing <- Pricing.makeClient { region: Region "us-east-1" }
  _ <-
    fromAff do
      -- let 
      --   filters :: Array Filter
      --   filters = [ ResourceType [ Instance ] ]
      let
        filterLocation =
          { type: TERM_MATCH
          , field: FilterField "location"
          , value: FilterValue "EU (Frankfurt)"
          }
        -- ECS
        filterUsageType =
          { type: TERM_MATCH
          , field: FilterField "UsageType"
          , value: FilterValue (show EUC1FargateGBHours)
          }
        -- EC2
        filterInstanceType=
          { type: TERM_MATCH
          , field: FilterField "instanceType"
          , value: FilterValue ("c5.2xlarge")
          }
        filterOperatingSystem=
          { type: TERM_MATCH
          , field: FilterField "operatingSystem"
          , value: FilterValue "Linux"  
            -- no possibility to find out which operatingSystem is runnning on a given instanceId so far 
            -- so we will have to query prices for different OperatingSystem and give an approximative price
          }
        filterUsagetype=
          { type: TERM_MATCH
          , field: FilterField "usagetype"
          , value: FilterValue "EUC1-BoxUsage:c5.2xlarge"
          }
        filterPreInstalledSw=
          { type: TERM_MATCH
          , field: FilterField "preInstalledSw"
          , value: FilterValue "NA"
          }

      -- res <- describeTags ec2 filters
      -- res <- describeTasks ecs [ TaskArn "arn:aws:ecs:eu-central-1:677840207937:task/cbis-production-microservices/7e66abc54a4b457399417f57d77a4582" ] (ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices")
      -- res <- listClusters ecs 
      -- res <- listTasks ecs (ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices") (ContainerInstanceArn "arn:aws:ecs:eu-central-1:677840207937:container-instance/cbis-production-microservices/59f156757059479f8c1a8a742174c15c")
      -- res <- listContainerInstances ecs (ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices")
      -- res <- describeClusters ecs [ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices"]
      -- res <- describeContainerInstances ecs (ClusterArn "arn:aws:ecs:eu-central-1:677840207937:cluster/cbis-production-microservices")  [ContainerInstanceArn "arn:aws:ecs:eu-central-1:677840207937:container-instance/cbis-production-microservices/59f156757059479f8c1a8a742174c15c"]
  
      res <- getAllEC2Products 
        pricing 
        [ 
          filterLocation
          , filterInstanceType
          , filterOperatingSystem
          , filterUsagetype
          , filterPreInstalledSw
        ] 
      -- res <- getECSProducts pricing [ filterLocation, filfilterUsageType ] AmazonECS Nothing (Just 10.0)
      _ <- log $ show res
      -- _ <- log $ show res
      -- _ <- case res of
      --   Left e -> log e
      --   Right r -> log $ show r
      logShow "Done fromAff"
  logShow "Done do"
