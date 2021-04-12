module AWS.ECS
  ( ECS
  , makeClient
  , listClusters
  ) where

import Prelude

import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import AWS.ECS.Types (ListClustersResponse)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))

foreign import data ECS :: Type

foreign import newECS :: Json -> (Effect ECS)

makeClient ::
  forall r via.
  Justifiable { | r } { | via } =>
  Fillable { | via } DefaultClientProps =>
  { | r } ->
  Effect ECS
makeClient r = makeClientHelper newECS props
  where
  viaProxy :: Proxy { | via }
  viaProxy = Proxy

  props :: DefaultClientProps
  props = justifillVia viaProxy r

type InternalListClustersResponse
  = { clusterArns :: Array String }

foreign import listClustersImpl :: Fn1 ECS (Effect (Promise InternalListClustersResponse))

listClusters :: ECS -> Aff ListClustersResponse
listClusters ecs =
  runFn1 listClustersImpl ecs
    # toAffE
    <#> toResponse
  where
  toResponse :: InternalListClustersResponse -> ListClustersResponse
  toResponse internalRspse = { clusterArns: internalRspse."clusterArns" <#> wrap }
