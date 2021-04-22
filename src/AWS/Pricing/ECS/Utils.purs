module AWS.Pricing.ECS.Utils where

import Prelude
import AWS.Core.Types (Region)
import AWS.Core.Util (handleError)
import AWS.Pricing.ECS.Types (ECSAttributes, ECSPriceList, ECSProduct, ECSUsageType(..))
import AWS.Pricing.Types (FilterValue(..), InternalPriceList, InternalProduct)
import AWS.Pricing.Utils (toTerms)
import Data.Argonaut (Json, decodeJson)
import Data.Bifunctor (lmap, bimap)
import Data.Either (Either)
import Data.Newtype (unwrap)

toECSPriceList :: InternalPriceList -> ECSPriceList
toECSPriceList pl =
  { serviceCode: pl.serviceCode
  , version: pl.version
  , publicationDate: pl.publicationDate
  , product: toECSProduct pl.product
  , terms: toTerms pl.terms
  }

toECSProduct :: InternalProduct -> ECSProduct ()
toECSProduct internalProduct =
  { attributes: parseAttributes internalProduct.attributes
  }
  where
  parseAttributes :: Json -> Either String (ECSAttributes ())
  parseAttributes = decodeJson <#> lmap handleError

parseECSPriceList :: Json -> Either String ECSPriceList
parseECSPriceList = decodeJson <#> bimap handleError toECSPriceList

toFargateGBHours :: Region -> FilterValue
toFargateGBHours region = case unwrap region of
  "eu-central-1" -> FilterValue $ show EUC1FargateGBHours
  "eu-west-1" -> FilterValue $ show EUFargateGBHours
  _ -> FilterValue $ "Unkown region" -- default value or error case?

toFargatevCPUHours :: Region -> FilterValue
toFargatevCPUHours region = case unwrap region of
  "eu-central-1" -> FilterValue $ show EUC1FargatevCPUHoursperCPU
  "eu-west-1" -> FilterValue $ show EUFargatevCPUHoursperCPU
  _ -> FilterValue $ "Unkown region" -- default value or error case?
