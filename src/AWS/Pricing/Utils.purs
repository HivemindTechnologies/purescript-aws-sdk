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
  _ -> FilterValue "EU (Frankfurt)"
