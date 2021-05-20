module AWS.Pricing
  ( Pricing
  , makeClient
  , getProducts
  , getAllProducts
  ) where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import AWS.Core.Util (unfoldrM1)
import AWS.Pricing.Types (Filter, GetProductsResponse, PriceList, ServiceCode)
import AWS.Pricing.Utils (parsePriceList)
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

getProducts ::
  Pricing ->
  Array Filter ->
  ServiceCode ->
  Maybe String ->
  Maybe Number ->
  Aff (GetProductsResponse)
getProducts pricing filters serviceCode token max =
  ( toAffE
      $ runFn5 getProductsImpl
          pricing
          (filters <#> toInternalFilter)
          (unwrap serviceCode)
          (Nullable.toNullable token)
          (Nullable.toNullable max)
  )
    <#> toResponse
  where
  toInternalFilter :: Filter -> InternalFilter
  toInternalFilter filter =
    { "Field": unwrap filter.field
    , "Type": show filter.type
    , "Value": unwrap filter.value
    }

  toResponse :: InternalGetProductsResponse -> GetProductsResponse
  toResponse internal =
    { formatVersion: internal."FormatVersion"
    , priceList: internal."PriceList" <#> parsePriceList
    , nextToken: Nullable.toMaybe internal."NextToken"
    }

getAllProducts ::
  Pricing ->
  Array Filter ->
  ServiceCode ->
  Aff (Array (Either String PriceList))
getAllProducts api filters serviceCode = do
  initial :: GetProductsResponse <- getProducts api filters serviceCode Nothing Nothing
  next :: Array (Array (Either String PriceList)) <- fetchAllNext initial.nextToken
  let
    all :: Array (Array (Either String PriceList))
    all = pure initial.priceList <> next

    allFlatten :: Array (Either String PriceList)
    allFlatten = all # join
  pure allFlatten
  where
  -- func
  getProductsAndNextToken ::
    String ->
    Aff
      ( Tuple
          (Array (Either String PriceList))
          (Maybe String)
      )
  getProductsAndNextToken currentNextToken = do
    products <- getProducts api filters serviceCode (Just currentNextToken) Nothing
    let
      nextToken = products.nextToken

      priceList = products.priceList
    pure $ Tuple priceList nextToken

  fetchAllNext :: Maybe String -> Aff (Array (Array (Either String PriceList)))
  fetchAllNext token = unfoldrM1 token getProductsAndNextToken
