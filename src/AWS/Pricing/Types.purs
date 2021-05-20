module AWS.Pricing.Types where

import Prelude
import AWS.Core.Types (decodeAsMap)
import Data.Argonaut (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.DateTime (DateTime)
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

type InternalPriceDimension r
  = { description :: String
    , unit :: String
    , pricePerUnit :: { "USD" :: String }
    | r
    }

newtype InternalPriceDimensions
  = InternalPriceDimensions (Map.Map String (InternalPriceDimension ()))

derive newtype instance showPriceDimensions :: Show InternalPriceDimensions

derive instance ntPriceDimensions :: Newtype InternalPriceDimensions _

instance priceDimensionsDecoder :: DecodeJson InternalPriceDimensions where
  decodeJson = decodeAsMap >>> map InternalPriceDimensions

newtype InternalOnDemand
  = InternalOnDemand (Map.Map String (InternalPriceDetails ()))

derive newtype instance showOnDemand :: Show InternalOnDemand

derive instance ntOnDemand :: Newtype InternalOnDemand _

instance onDemandDecoder :: DecodeJson InternalOnDemand where
  decodeJson = decodeAsMap >>> map InternalOnDemand

type InternalPriceDetails r
  = { priceDimensions :: InternalPriceDimensions
    , effectiveDate :: String
    | r
    }

-- there is alos "Reserved" prices
-- but at the time of writing we don't need that information
-- type InternalTerms r
type InternalTerms r
  = { "OnDemand" :: InternalOnDemand
    | r
    }

type InternalPriceList
  = { serviceCode :: String
    , version :: String
    , publicationDate :: String
    , product :: Product ()
    , terms :: InternalTerms ()
    }

type PriceList
  = { serviceCode :: String
    , version :: String
    , publicationDate :: String
    , product :: Product ()
    , terms :: Terms
    }

type Terms
  = { "OnDemand" :: OnDemand }

newtype OnDemand
  = OnDemand (Map.Map String PriceDetails)

derive newtype instance showOnDemandA :: Show OnDemand

derive instance ntOnDemandA :: Newtype OnDemand _

type PriceDetails
  = { priceDimensions :: PriceDimensions
    , effectiveDate :: Maybe DateTime
    }

newtype PriceDimensions
  = PriceDimensions (Map.Map String PriceDimension)

derive newtype instance showPriceDimensionsA :: Show PriceDimensions

derive instance ntPriceDimensionsA :: Newtype PriceDimensions _

type PriceDimension
  = { description :: String
    , unit :: PriceUnit
    , pricePerUnit :: { "USD" :: String }
    }

data PriceUnit
  = Hours
  | Quantity

instance encodeUnit :: EncodeJson PriceUnit where
  encodeJson Hours = encodeString "Hrs"
  encodeJson Quantity = encodeString "Quantity"

instance showUnit :: Show PriceUnit where
  show Hours = "Hrs"
  show Quantity = "Quantity"

derive instance eqUnit :: Eq PriceUnit

toUnit :: String -> PriceUnit
toUnit unit = case unit of
  "Hrs" -> Hours
  "Quantity" -> Quantity
  _ -> Hours
