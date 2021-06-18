module AWS.Pricing.ECS.Utils where

import Prelude
import AWS.Core.Util (handleError)
import AWS.Pricing.ECS.Types
  ( ECSPriceList
  , ECSProduct
  , ECSAttributes
  )
import AWS.Pricing.Types (InternalPriceList, InternalProduct)
import AWS.Pricing.Utils (toTerms)
import Data.Argonaut (Json, decodeJson)
import Data.Bifunctor (lmap, bimap)
import Data.Either (Either)

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
