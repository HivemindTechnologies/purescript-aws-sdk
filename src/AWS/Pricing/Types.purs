module AWS.Pricing.Types where

import Prelude
import AWS.Core.Types (decodeAsMap)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Hours)
import Foreign.Object as F

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

------
type PriceListA
  = { serviceCode :: String
    , version :: String
    , publicationDate :: String
    , product :: Product ()
    , terms :: TermsA
    }

type TermsA
  = { "OnDemand" :: OnDemandA }

newtype OnDemandA
  = OnDemandA (Map.Map String PriceDetailsA)

derive newtype instance showOnDemandA :: Show OnDemandA

derive instance ntOnDemandA :: Newtype OnDemandA _

-- instance onDemandADecoder :: DecodeJson OnDemandA where
--   decodeJson = decodeAsMapA >>> map OnDemandA
type PriceDetailsA
  = { priceDimensions :: PriceDimensionsA
    , effectiveDate :: Maybe DateTime
    }

newtype PriceDimensionsA
  = PriceDimensionsA (Map.Map String PriceDimensionA)

derive newtype instance showPriceDimensionsA :: Show PriceDimensionsA

derive instance ntPriceDimensionsA :: Newtype PriceDimensionsA _

-- instance priceDimensionsADecoder :: DecodeJson PriceDimensionsA where
--   decodeJson = decodeAsMapA >>> map PriceDimensionsA
type PriceDimensionA
  = { description :: String
    , unit :: Unit
    , pricePerUnit :: { "USD" :: String }
    }

data Unit
  = Hours
  | Quantity

instance encodeUnit :: EncodeJson Unit where
  encodeJson Hours = encodeString "Hrs"
  encodeJson Quantity = encodeString "Quantity"

instance showUnit :: Show Unit where
  show Hours = "Hrs"
  show Quantity = "Quantity"

toUnit :: String -> Unit
toUnit unit = case unit of
  "Hrs" -> Hours
  "Quantity" -> Quantity
  _ -> Hours

-- decodeAsMapA :: forall r. DecodeJson r => Json -> Either JsonDecodeError (Map.Map String r)
-- decodeAsMapA str = do
--   obj <- decodeJson str
--   pure $ Map.fromFoldable $ (F.toUnfoldable obj :: Array _)
