module AWS.Pricing.Utils where

import Prelude
import AWS.Core.Types (Region)
import AWS.Pricing.Types (FilterValue(..), InternalOnDemand, InternalPriceDetails, InternalPriceDimension, InternalTerms, OnDemand(..), PriceDetails, PriceDimension, PriceDimensions(..), Terms, toUnit)
import Data.DateTime (DateTime)
import Data.Either (Either, hush)
import Data.Formatter.DateTime (unformatDateTime)
import Data.Newtype (unwrap)

toTerms :: InternalTerms () -> Terms
toTerms terms = { "OnDemand": toOnDemand terms."OnDemand" }

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

toLocation :: Region -> FilterValue
toLocation r = case unwrap r of
  "eu-central-1" -> FilterValue "EU (Frankfurt)"
  "eu-west-1" -> FilterValue "EU (Ireland)"
  _ -> FilterValue "Unknown region" -- default value or error case?

toBoxUsage :: Region -> String -> FilterValue
toBoxUsage region instanceType = case unwrap region of
  "eu-central-1" -> FilterValue $ "EUC1-BoxUsage:" <> instanceType
  "eu-west-1" -> FilterValue $ "EU-BoxUsage:" <> instanceType
  _ -> FilterValue $ "Unkown region" -- default value or error case?

{- 
Other possible values are:
"EU-Reservation:"
"EU-UnusedBox:"
"EU-DedicatedRes:"
"EU-BoxUsage:"
"EU-UnusedDed:"
"EU-DedicatedUsage:"
"EU-HostBoxUsage:"
-}
