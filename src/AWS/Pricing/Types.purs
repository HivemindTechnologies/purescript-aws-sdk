module AWS.Pricing.Types where

import Prelude
import AWS.Core.Types (decodeAsMap)
import Data.Argonaut (class DecodeJson)
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

data FilterType
  = TERM_MATCH

instance showFilterType :: Show FilterType where
  show TERM_MATCH = "TERM_MATCH"

newtype FilterField
  = FilterField String

derive instance ntFilterField :: Newtype FilterField _

newtype FilterValue
  = FilterValue String

derive instance ntFilterValue :: Newtype FilterValue _

type Filter
  = { field :: FilterField
    , type :: FilterType
    , value :: FilterValue
    }

newtype ServiceCode
  = ServiceCode String

derive instance ntServiceCode :: Newtype ServiceCode _

type GetProductsResponse
  = { formatVersion :: String
    , priceList :: Array (Either String PriceList)
    , nextToken :: Maybe String
    }

type Attributes r
  = { instanceType :: String | r }

type Product r
  = { attributes :: Attributes () | r }

type PriceDimension r
  = { description :: String
    , unit :: String
    , pricePerUnit :: { "USD" :: String }
    | r
    }

newtype PriceDimensions
  = PriceDimensions (Map.Map String (PriceDimension ()))

derive newtype instance showPriceDimensions :: Show PriceDimensions

derive instance ntPriceDimensions :: Newtype PriceDimensions _

instance priceDimensionsDecoder :: DecodeJson PriceDimensions where
  decodeJson = decodeAsMap >>> map PriceDimensions

newtype OnDemand
  = OnDemand (Map.Map String (PriceDetails ()))

derive newtype instance showOnDemand :: Show OnDemand

derive instance ntOnDemand :: Newtype OnDemand _

instance onDemandDecoder :: DecodeJson OnDemand where
  decodeJson = decodeAsMap >>> map OnDemand

type PriceDetails r
  = { priceDimensions :: PriceDimensions
    , effectiveDate :: String
    | r
    }

-- there is alos "Reserved" prices
-- but at the time of writing we don't need that information
-- type Terms r
--   = { "OnDemand" :: OnDemandPrice | r }
type Terms r
  = { "OnDemand" :: OnDemand
    | r
    }

type PriceList
  = { serviceCode :: String
    , version :: String
    , publicationDate :: String
    , product :: Product ()
    , terms :: Terms ()
    }
