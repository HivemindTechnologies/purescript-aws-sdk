module AWS.Pricing.Utils where

import Prelude
import AWS.Core.Types (Region)
import AWS.Core.Util (handleError)
import AWS.Pricing.Types
  ( FilterValue(..)
  , InternalOnDemand
  , InternalPriceDetails
  , InternalPriceDimension
  , InternalPriceList
  , InternalProduct
  , InternalTerms
  , OnDemand(..)
  , PriceDetails
  , PriceDimension
  , PriceDimensions(..)
  , EC2PriceList
  , ECSPriceList
  , EC2Product
  , ECSProduct
  , Terms
  , toUnit
  )
import Data.Argonaut (Json, decodeJson)
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime)
import Data.Either (Either, hush)
import Data.Formatter.DateTime (unformatDateTime)
import Data.Newtype (unwrap)

toEC2PriceList :: InternalPriceList -> EC2PriceList
toEC2PriceList pl =
  { serviceCode: pl.serviceCode
  , version: pl.version
  , publicationDate: pl.publicationDate
  , product: toEC2Product pl.product
  , terms: toTerms pl.terms
  }

toECSPriceList :: InternalPriceList -> ECSPriceList
toECSPriceList pl =
  { serviceCode: pl.serviceCode
  , version: pl.version
  , publicationDate: pl.publicationDate
  , product: toECSProduct pl.product
  , terms: toTerms pl.terms
  }

toTerms :: InternalTerms () -> Terms
toTerms terms = { "OnDemand": toOnDemand terms."OnDemand" }

toEC2Product :: InternalProduct -> EC2Product ()
toEC2Product internalProduct =
  { attributes: ec2Attributes
  }
  where
  ec2Attributes =
    { instanceType: ""
    , instanceFamily: ""
    , operatingSystem: ""
    , vcpu: ""
    }

toECSProduct :: InternalProduct -> ECSProduct ()
toECSProduct internalProduct =
  { attributes: ecsAttributes
  }
  where
  ecsAttributes =
    { servicecode: ""
    , usagetype: ""
    , servicename: ""
    , operation: ""
    , storagetype: ""
    }

toOnDemand :: InternalOnDemand -> OnDemand
toOnDemand onDemand = (unwrap onDemand) <#> toPriceDetails # OnDemand

toPriceDetails :: InternalPriceDetails () -> PriceDetails
toPriceDetails priceDetails =
  { priceDimensions:
      (unwrap priceDetails.priceDimensions)
        <#> toPriceDimension
        # PriceDimensions
  , effectiveDate: hush $ parseDateTime priceDetails.effectiveDate
  }

toPriceDimension :: InternalPriceDimension () -> PriceDimension
toPriceDimension priceDimension =
  { description: priceDimension.description
  , unit: toUnit priceDimension.unit
  , pricePerUnit: priceDimension.pricePerUnit
  }

parseDateTime :: String -> Either String DateTime
parseDateTime = unformatDateTime "YYYY-MM-DDTHH:mm:ssZ"

parseEC2PriceList :: Json -> Either String EC2PriceList
parseEC2PriceList = decodeJson <#> bimap handleError toEC2PriceList

parseECSPriceList :: Json -> Either String ECSPriceList
parseECSPriceList = decodeJson <#> bimap handleError toECSPriceList

toLocation :: Region -> FilterValue
toLocation r = case unwrap r of
  "eu-central-1" -> FilterValue "EU (Frankfurt)"
  "eu-west-1" -> FilterValue "EU (Ireland)"
  _ -> FilterValue "EU (Frankfurt)"
