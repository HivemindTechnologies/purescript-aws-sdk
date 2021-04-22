module AWS.Pricing.EC2.Utils where

import Prelude
import AWS.Core.Util (handleError)
import AWS.Pricing.EC2.Types (EC2Attributes, EC2PriceList, EC2Product)
import AWS.Pricing.Types (InternalPriceList, InternalProduct)
import AWS.Pricing.Utils (toTerms)
import Data.Argonaut (Json, decodeJson)
import Data.Bifunctor (lmap, bimap)
import Data.Either (Either)

toEC2PriceList :: InternalPriceList -> EC2PriceList
toEC2PriceList pl =
  { serviceCode: pl.serviceCode
  , version: pl.version
  , publicationDate: pl.publicationDate
  , product: toEC2Product pl.product
  , terms: toTerms pl.terms
  }

toEC2Product :: InternalProduct -> EC2Product ()
toEC2Product internalProduct =
  { attributes: parseAttributes internalProduct.attributes
  }
  where
  parseAttributes :: Json -> Either String (EC2Attributes ())
  parseAttributes = decodeJson <#> lmap handleError

parseEC2PriceList :: Json -> Either String EC2PriceList
parseEC2PriceList = decodeJson <#> bimap handleError toEC2PriceList
