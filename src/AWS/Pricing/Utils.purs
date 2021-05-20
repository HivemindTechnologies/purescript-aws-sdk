module AWS.Pricing.Utils where

import Prelude
import AWS.Core.Util (handleError)
import AWS.Pricing.Types (InternalOnDemand, InternalPriceDetails, InternalPriceDimension, InternalPriceList, InternalTerms, OnDemand(..), PriceDetails, PriceDimension, PriceDimensions(..), PriceList, Terms, toUnit)
import Data.Argonaut (Json, decodeJson)
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime)
import Data.Either (Either, hush)
import Data.Formatter.DateTime (unformatDateTime)
import Data.Newtype (unwrap)

toPriceList :: InternalPriceList -> PriceList
toPriceList pl =
  { serviceCode: pl.serviceCode
  , version: pl.version
  , publicationDate: pl.publicationDate
  , product: pl.product
  , terms: toTerms pl.terms
  }

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

parsePriceList :: Json -> Either String PriceList
parsePriceList = decodeJson <#> bimap handleError toPriceList
