module AWS.Pricing.Types where

import Prelude
import Data.Either (Either)
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

type PriceList r
  = { serviceCode :: String
    , version :: String
    , publicationDate :: String
    | r
    }

type GetProductsResponse
  = { formatVersion :: String
    , priceList :: Maybe (Array (PriceList ()))
    , nextToken :: Maybe String
    }
