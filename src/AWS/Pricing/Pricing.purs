module AWS.Pricing
  ( Pricing
  , makeClient
  , getEC2Products
  , getECSProducts
  , getAllEC2Products
  ) where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import AWS.Core.Util (unfoldrM1)
import AWS.Pricing.Types (Filter, GetEC2ProductsResponse, GetECSProductsResponse, EC2PriceList, ServiceCode)
import AWS.Pricing.Utils (parseEC2PriceList, parseECSPriceList)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json)
import Data.Either (Either)
import Data.Function.Uncurried (Fn5, runFn5)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))

foreign import data Pricing :: Type

foreign import newPricing :: Json -> Effect Pricing

makeClient ::
  forall r via.
  Justifiable { | r } { | via } =>
  Fillable { | via } DefaultClientProps =>
  { | r } ->
  Effect Pricing
makeClient r = makeClientHelper newPricing props
  where
  viaProxy :: Proxy { | via }
  viaProxy = Proxy

  props :: DefaultClientProps
  props = justifillVia viaProxy r

type InternalFilter
  = { "Field" :: String
    , "Type" :: String
    , "Value" :: String
    }

type InternalGetProductsResponse
  = { "FormatVersion" :: String
    , "PriceList" :: Array Json
    , "NextToken" :: Nullable String
    }

foreign import getProductsImpl ::
  Fn5
    Pricing
    (Array InternalFilter)
    String
    (Nullable String)
    (Nullable Number)
    (Effect (Promise InternalGetProductsResponse))

curried ::
  Pricing ->
  Array Filter ->
  ServiceCode ->
  Maybe String ->
  Maybe Number ->
  Aff InternalGetProductsResponse
curried pricing filters serviceCode token max =
  ( toAffE
      $ runFn5 getProductsImpl
          pricing
          (filters <#> toInternalFilter)
          (show serviceCode)
          (Nullable.toNullable token)
          (Nullable.toNullable max)
  )
  where
  toInternalFilter :: Filter -> InternalFilter
  toInternalFilter filter =
    { "Field": unwrap filter.field
    , "Type": show filter.type
    , "Value": unwrap filter.value
    }

getEC2Products ::
  Pricing ->
  Array Filter ->
  ServiceCode ->
  Maybe String ->
  Maybe Number ->
  Aff GetEC2ProductsResponse
getEC2Products pricing filters serviceCode token max = curried pricing filters serviceCode token max <#> toResponse
  where
  toResponse :: InternalGetProductsResponse -> GetEC2ProductsResponse
  toResponse internal =
    { formatVersion: internal."FormatVersion"
    , priceList: internal."PriceList" <#> parseEC2PriceList
    , nextToken: Nullable.toMaybe internal."NextToken"
    }

getECSProducts ::
  Pricing ->
  Array Filter ->
  ServiceCode ->
  Maybe String ->
  Maybe Number ->
  Aff GetECSProductsResponse
getECSProducts pricing filters serviceCode token max = curried pricing filters serviceCode token max <#> toResponse
  where
  toResponse :: InternalGetProductsResponse -> GetECSProductsResponse
  toResponse internal =
    { formatVersion: internal."FormatVersion"
    , priceList: internal."PriceList" <#> parseECSPriceList
    , nextToken: Nullable.toMaybe internal."NextToken"
    }

getAllEC2Products ::
  Pricing ->
  Array Filter ->
  ServiceCode ->
  Aff (Array (Either String EC2PriceList))
getAllEC2Products api filters serviceCode = do
  initial :: GetEC2ProductsResponse <- getEC2Products api filters serviceCode Nothing Nothing
  next :: Array (Array (Either String EC2PriceList)) <- fetchAllNext initial.nextToken
  let
    all :: Array (Array (Either String EC2PriceList))
    all = pure initial.priceList <> next
    allFlatten :: Array (Either String EC2PriceList)
    allFlatten = all # join
  pure allFlatten
  where
  -- func
  getProductsAndNextToken ::
    String ->
    Aff
      ( Tuple
          (Array (Either String EC2PriceList))
          (Maybe String)
      )
  getProductsAndNextToken currentNextToken = do
    products <- getEC2Products api filters serviceCode (Just currentNextToken) Nothing
    let
      nextToken = products.nextToken
      priceList = products.priceList
    pure $ Tuple priceList nextToken
  fetchAllNext :: Maybe String -> Aff (Array (Array (Either String EC2PriceList)))
  fetchAllNext token = unfoldrM1 token getProductsAndNextToken
