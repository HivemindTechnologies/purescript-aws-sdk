module AWS.ECS
  ( ECS
  , makeClient
  , listClusters
  , listTasks
  ) where

import Prelude

import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import AWS.ECS.Types (ClusterArn(..), ListClustersResponse, ListTasksResponse)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Newtype (un, wrap)
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

foreign import listTasksImpl :: Fn2 ECS String (Effect (Promise InternalListTasksResponse))

type InternalListTasksResponse
  = { taskArns :: Array String }

listTasks :: ECS -> ClusterArn -> Aff ListTasksResponse
listTasks ecs clusterArn =
  runFn2 listTasksImpl ecs (un ClusterArn clusterArn)
    # toAffE
    <#> toResponse
  where
  toResponse :: InternalListTasksResponse -> ListTasksResponse
  toResponse internalRspse = { taskArns: internalRspse."taskArns" <#> wrap }
