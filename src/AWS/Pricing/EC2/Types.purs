module AWS.Pricing.EC2.Types where

import AWS.Pricing.Types (Terms)
import Data.Either (Either)
import Data.Maybe (Maybe)

type GetEC2ProductsResponse
  = { formatVersion :: String
    , priceList :: Array (Either String EC2PriceList)
    , nextToken :: Maybe String
    }

type EC2Attributes r
  = { instanceType :: String
    , instanceFamily :: String
    , operatingSystem :: String
    , vcpu :: String
    , servicename :: String
    | r
    }

type EC2Product r
  = { attributes :: (Either String (EC2Attributes ())) | r }

type EC2PriceList
  = { serviceCode :: String
    , version :: String
    , publicationDate :: String
    , product :: EC2Product ()
    , terms :: Terms
    }
