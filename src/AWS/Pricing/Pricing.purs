module AWS.Pricing
  ( Pricing
  , makeClient
  , getProducts
  , getAllProducts
  ) where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import AWS.Core.Util (handleError, unfoldrM1)
import AWS.Pricing.Types (Filter, GetProductsResponse, OnDemand, OnDemandA(..), PriceDetails, PriceDetailsA, PriceDimension, PriceDimensionA, PriceDimensionsA(..), PriceList, PriceListA, ServiceCode, Terms, TermsA, toUnit)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json, decodeJson)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Either (Either, hush)
import Data.Formatter.DateTime (unformatDateTime)
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

foreign import getProductsImpl :: Fn5 Pricing (Array InternalFilter) String (Nullable String) (Nullable Number) (Effect (Promise InternalGetProductsResponse))

getProducts :: Pricing -> Array Filter -> ServiceCode -> Maybe String -> Maybe Number -> Aff (GetProductsResponse)
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

  toPriceList :: Json -> Either String PriceList
  toPriceList = decodeJson <#> lmap handleError

  toPLA :: Either String PriceList -> Either String PriceListA
  toPLA e = e <#> toPriceListA

  foo :: Json -> Either String PriceListA
  foo = toPriceList >>> toPLA

  toResponse :: InternalGetProductsResponse -> GetProductsResponse
  toResponse internal =
    { formatVersion: internal."FormatVersion"
    , priceList: internal."PriceList" <#> foo
    , nextToken: Nullable.toMaybe internal."NextToken"
    }

type InternalGetProductsResponse
  = { "FormatVersion" :: String
    , "PriceList" :: Array Json
    , "NextToken" :: Nullable String
    }

getAllProducts ::
  Pricing ->
  Array Filter ->
  ServiceCode ->
  Aff (Array (Either String PriceListA))
getAllProducts api filters serviceCode = do
  initial :: GetProductsResponse <- getProducts api filters serviceCode Nothing Nothing
  next :: Array (Array (Either String PriceListA)) <- fetchAllNext initial.nextToken
  let
    all :: Array (Array (Either String PriceListA))
    all = pure initial.priceList <> next

    allFlatten :: Array (Either String PriceListA)
    allFlatten = all # join
  pure allFlatten
  where
  -- func
  getProductsAndNextToken :: String -> Aff (Tuple (Array (Either String PriceListA)) (Maybe String))
  getProductsAndNextToken currentNextToken = do
    products <- getProducts api filters serviceCode (Just currentNextToken) Nothing
    let
      nextToken = products.nextToken

      priceList = products.priceList
    pure $ Tuple priceList nextToken

  fetchAllNext :: Maybe String -> Aff (Array (Array (Either String PriceListA)))
  fetchAllNext token = unfoldrM1 token getProductsAndNextToken

toPriceListA :: PriceList -> PriceListA
toPriceListA pl =
  { serviceCode: pl.serviceCode
  , version: pl.version
  , publicationDate: pl.publicationDate
  , product: pl.product
  , terms: toTermsA pl.terms
  }

toTermsA :: Terms () -> TermsA
toTermsA terms = { "OnDemand": toOnDemandA terms."OnDemand" }

toOnDemandA :: OnDemand -> OnDemandA
toOnDemandA onDemand = (unwrap onDemand) <#> toPriceDetailsA # OnDemandA

toPriceDetailsA :: PriceDetails () -> PriceDetailsA
toPriceDetailsA priceDetails =
  { priceDimensions:
      (unwrap priceDetails.priceDimensions)
        <#> toPriceDimensionA
        # PriceDimensionsA
  , effectiveDate: hush $ parseDateTime priceDetails.effectiveDate
  }

toPriceDimensionA :: PriceDimension () -> PriceDimensionA
toPriceDimensionA priceDimension =
  { description: priceDimension.description
  , unit: toUnit priceDimension.unit
  , pricePerUnit: priceDimension.pricePerUnit
  }

parseDateTime :: String -> Either String DateTime
parseDateTime = unformatDateTime "YYYY-MM-DDTHH:mm:ssZ"
