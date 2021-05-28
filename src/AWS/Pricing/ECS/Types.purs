module AWS.Pricing.ECS.Types where

import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe)
import AWS.Pricing.Types (Terms)

type GetECSProductsResponse
  = { formatVersion :: String
    , priceList :: Array (Either String ECSPriceList)
    , nextToken :: Maybe String
    }

type ECSAttributes r
  = { servicecode :: String
    , usagetype :: String
    , servicename :: String
    , operation :: String
    | r
    }

type ECSProduct r
  = { attributes :: (Either String (ECSAttributes ())) | r }

type ECSPriceList
  = { serviceCode :: String
    , version :: String
    , publicationDate :: String
    , product :: ECSProduct ()
    , terms :: Terms
    }

data ECSUsageType
  = EUC1FargateEphemeralStorageGBHours
  | EUC1FargateGBHours
  | EUC1FargatevCPUHoursperCPU

instance showECSUsageType :: Show ECSUsageType where
  show EUC1FargateEphemeralStorageGBHours = "EUC1-Fargate-EphemeralStorage-GB-Hours"
  show EUC1FargateGBHours = "EUC1-Fargate-GB-Hours"
  show EUC1FargatevCPUHoursperCPU = "EUC1-Fargate-vCPU-Hours:perCPU"
