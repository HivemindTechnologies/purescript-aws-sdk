module AWS.EC2
  ( EC2
  , makeClient
  , describeInstances
  , Filter(..)
  , ResourceTypeName(..)
  , describeTags
  , DescribeTagsResponse
  , Tag
  , describeInstanceAttributeInstanceType
  , newEC2
  , describeInstanceTypes
  , InstanceStateName(..)
  ) where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps, Instance, InstanceId(..), InstanceType(..))
import AWS.Core.Util (handleError)
import AWS.EC2.Types (Attribute(..)) as Attribute
import AWS.EC2.Types (Attribute, InstanceAttributeInstanceType, DescribeInstanceTypesResponse)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))

foreign import data EC2 :: Type

foreign import newEC2 :: Json -> (Effect EC2)

makeClient ::
  forall r via.
  Justifiable { | r } { | via } =>
  Fillable { | via } DefaultClientProps =>
  { | r } ->
  Effect EC2
makeClient r = makeClientHelper newEC2 props
  where
  viaProxy :: Proxy { | via }
  viaProxy = Proxy

  props :: DefaultClientProps
  props = justifillVia viaProxy r

type InternalEC2Instance
  = { "InstanceId" :: String, "InstanceType" :: String }

type InternalEC2Reservation
  = { "Instances" :: Array InternalEC2Instance }

type InternalEC2Response
  = { "Reservations" :: Array InternalEC2Reservation }

foreign import describeInstancesImpl :: Fn2 EC2 (Array InternalFilter) (Effect (Promise InternalEC2Response))

describeInstances :: EC2 -> Array Filter -> Aff (Array Instance)
describeInstances ec2 filters = runFn2 describeInstancesImpl ec2 internalFilters # toAffE <#> toExternal
  where
  toExternal :: InternalEC2Response -> Array Instance
  toExternal response = do
    reservations <- response."Reservations"
    instances <- reservations."Instances"
    pure
      { id: InstanceId instances."InstanceId"
      , "type": InstanceType instances."InstanceType"
      }

  internalFilters :: Array InternalFilter
  internalFilters = filters <#> toInternalFilter

data ResourceTypeName
  = CustomerGateway
  | DedicatedHost
  | DhcpOptions
  | ElasticIp
  | Fleet
  | FpgaImage
  | HostReservation
  | Image
  | Instance
  | InternetGateway
  | KeyPair
  | LaunchTemplate
  | Natgateway
  | NetworkAcl
  | NetworkInterface
  | PlacementGroup
  | ReservedInstances
  | RouteTable
  | SecurityGroup
  | Snapshot
  | SpotInstancesRequest
  | Subnet
  | Volume
  | Vpc
  | VpcEndpoint
  | VpcEndpointService
  | VpcPeeringConnection
  | VpnConnection
  | VpnGateway

toInternalResourceTypeName :: ResourceTypeName -> String
toInternalResourceTypeName resourceTypeName = case resourceTypeName of
  CustomerGateway -> "customer-gateway"
  DedicatedHost -> "dedicated-host"
  DhcpOptions -> "dhcp-options"
  ElasticIp -> "elastic-ip"
  Fleet -> "fleet"
  FpgaImage -> "fpga-image"
  HostReservation -> "host-reservation"
  Image -> "image"
  Instance -> "instance"
  InternetGateway -> "internet-gateway"
  KeyPair -> "key-pair"
  LaunchTemplate -> "launch-template"
  Natgateway -> "natgateway"
  NetworkAcl -> "network-acl"
  NetworkInterface -> "network-interface"
  PlacementGroup -> "placement-group"
  ReservedInstances -> "reserved-instances"
  RouteTable -> "route-table"
  SecurityGroup -> "security-group"
  Snapshot -> "snapshot"
  SpotInstancesRequest -> "spot-instances-request"
  Subnet -> "subnet"
  Volume -> "volume"
  Vpc -> "vpc"
  VpcEndpoint -> "vpc-endpoint"
  VpcEndpointService -> "vpc-endpoint-service"
  VpcPeeringConnection -> "vpc-peering-connection"
  VpnConnection -> "vpn-connection"
  VpnGateway -> "vpn-gateway"

data InstanceStateName
  = Pending
  | Running
  | ShuttingDown
  | Terminated
  | Stopping
  | Stopped

toInternalInstanceStateName :: InstanceStateName -> String
toInternalInstanceStateName state = case state of
  Pending -> "pending"
  Running -> "running"
  ShuttingDown -> "shutting-down"
  Terminated -> "terminated"
  Stopping -> "stopping"
  Stopped -> "stopped"

data Filter
  = Key (Array String)
  | ResourceId (Array String)
  | ResourceType (Array ResourceTypeName)
  | Tag String (Array String)
  | Value (Array String)
  | InstanceStateName (Array InstanceStateName)

type InternalFilter
  = { "Name" :: String, "Values" :: Array String }

toInternalFilter :: Filter -> InternalFilter
toInternalFilter filter = case filter of
  Key values -> { "Name": "key", "Values": values }
  ResourceId values -> { "Name": "resource-id", "Values": values }
  ResourceType values -> { "Name": "resource-type", "Values": values <#> toInternalResourceTypeName }
  Tag key values -> { "Name": "tag:" <> key, "Values": values }
  Value values -> { "Name": "value:", "Values": values }
  InstanceStateName values -> { "Name": "instance-state-name", "Values": values <#> toInternalInstanceStateName }

type InternalTag
  = { "ResourceType" :: String
    , "ResourceId" :: String
    , "Value" :: String
    , "Key" :: String
    }

type InternalDescribeTagsResponse
  = { "Tags" :: Array InternalTag
    }

type Tag
  = { resourceType :: String
    , resourceId :: String
    , value :: String
    , key :: String
    }

type DescribeTagsResponse
  = { tags :: Array Tag
    }

toTag :: InternalTag -> Tag
toTag internalTag =
  { resourceType: internalTag."ResourceType"
  , resourceId: internalTag."ResourceId"
  , value: internalTag."Value"
  , key: internalTag."Key"
  }

foreign import describeTagsImpl :: Fn2 EC2 (Array InternalFilter) (Effect (Promise InternalDescribeTagsResponse))

describeTags :: EC2 -> Array Filter -> Aff DescribeTagsResponse
describeTags ec2 filters =
  runFn2 describeTagsImpl ec2 internalFilters
    # toAffE
    <#> toResponse
  where
  internalFilters :: Array InternalFilter
  internalFilters = filters <#> toInternalFilter

  toResponse :: InternalDescribeTagsResponse -> DescribeTagsResponse
  toResponse internalRspse = { tags: internalRspse."Tags" <#> toTag }

foreign import describeInstanceAttributeImpl :: Fn3 EC2 String String (Effect (Promise Json))

curriedInstanceAttribute :: EC2 -> Attribute -> InstanceId -> Effect (Promise Json)
curriedInstanceAttribute ec2 attribute instanceId =
  runFn3 describeInstanceAttributeImpl
    ec2
    (show attribute)
    (unwrap instanceId)

describeInstanceAttributeInstanceType :: EC2 -> InstanceId -> Aff (Either String InstanceAttributeInstanceType)
describeInstanceAttributeInstanceType ec2 instanceId =
  (toAffE $ curriedInstanceAttribute ec2 Attribute.InstanceType instanceId)
    <#> parse
  where
  parse :: Json -> Either String InstanceAttributeInstanceType
  parse = decodeJson <#> lmap handleError

foreign import describeInstanceTypesImpl :: Fn2 EC2 (Array String) (Effect (Promise Json))

curriedInstanceTypes :: EC2 -> Array InstanceType -> Effect (Promise Json)
curriedInstanceTypes ec2 instanceTypes =
  runFn2 describeInstanceTypesImpl
    ec2
    (instanceTypes <#> unwrap)

describeInstanceTypes :: EC2 -> Array InstanceType -> Aff (Either String DescribeInstanceTypesResponse)
describeInstanceTypes ec2 instanceTypes =
  (toAffE $ curriedInstanceTypes ec2 instanceTypes)
    <#> parse
  where
  parse :: Json -> Either String (DescribeInstanceTypesResponse)
  parse = decodeJson <#> lmap handleError
