module AWS.ECS
  ( ECS
  , makeClient
  , listClusters
  , listTasks
  , listContainerInstances
  ) where

import Prelude

import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import AWS.ECS.Types (ClusterArn(..), ContainerInstanceArn(..), ListClustersResponse, ListContainerInstancesResponse, ListTasksResponse, Clusters, DescribeClustersResponse, ClusterParams)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Newtype (un, wrap, unwrap)
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

foreign import listTasksImpl :: Fn3 ECS String String (Effect (Promise InternalListTasksResponse))

type InternalListTasksResponse
  = { taskArns :: Array String }

listTasks :: ECS -> ClusterArn -> ContainerInstanceArn -> Aff ListTasksResponse
listTasks ecs clusterArn containerInstanceArn =
  runFn3 listTasksImpl ecs (un ClusterArn clusterArn) (un ContainerInstanceArn containerInstanceArn)
    # toAffE
    <#> toResponse
  where
  toResponse :: InternalListTasksResponse -> ListTasksResponse
  toResponse internalRspse = { taskArns: internalRspse."taskArns" <#> wrap }

foreign import listContainerInstancesImpl :: Fn2 ECS String (Effect (Promise InternalListContainerInstancesResponse))

type InternalListContainerInstancesResponse
  = { containerInstanceArns :: Array String }

listContainerInstances :: ECS -> ClusterArn -> Aff ListContainerInstancesResponse
listContainerInstances ecs clusterArn =
  runFn2 listContainerInstancesImpl ecs (un ClusterArn clusterArn)
    # toAffE
    <#> toResponse
  where
  toResponse :: InternalListContainerInstancesResponse -> ListContainerInstancesResponse
  toResponse internalRspse = { containerInstanceArns: internalRspse."containerInstanceArns" <#> wrap }

type InternalCluster r
  = { clusterArn :: String
    , clusterName :: String
    , status :: String
    , registeredContainerInstancesCount :: Number
    | r
    }

type InternalClusters r
  = Array (InternalCluster r)

type InternalDescribeClustersResponse r
  = { clusters :: InternalClusters r }

foreign import describeClustersImpl :: forall r. Fn2 ECS (Array String) (Effect (Promise (InternalDescribeClustersResponse r)))

describeClusters :: ECS -> Clusters -> Aff DescribeClustersResponse
describeClusters ecs clusterArns =
  runFn2 describeClustersImpl ecs (clusterArns <#> unwrap)
    # toAffE
    <#> toResponse
  where
  toResponse :: forall r. InternalDescribeClustersResponse r -> DescribeClustersResponse
  toResponse internalRspse = { clusters: internalRspse."clusters" <#> toDescribeClustersResponse }

  toDescribeClustersResponse :: forall r. InternalCluster r -> ClusterParams
  toDescribeClustersResponse i =
    { clusterArn: i.clusterArn
    , clusterName: i.clusterName
    , status: i.status
    , registeredContainerInstancesCount: i.registeredContainerInstancesCount
    }
