module AWS.EC2
  ( EC2
  , makeClient
  , describeInstances
  , Filter(..)
  , ResourceTypeName(..)
  , describeTags
  , DescribeTagsResponse
  , Tag
  , describeInstanceTypeAttribute
  , newEC2
  ) where

import Prelude

import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps, Instance, InstanceId(..), InstanceType(..))
import AWS.Core.Util (handleError)
import AWS.EC2.Types (Attribute, InstanceTypeAttribute)
import Control.Promise (Promise, toAffE)
import Control.Promise as Promise
import Data.Argonaut (Json, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn2, runFn3)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
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

foreign import describeInstancesImpl :: Fn1 EC2 (Effect (Promise InternalEC2Response))

describeInstances :: EC2 -> Aff (Array Instance)
describeInstances ec2 = map toExternal $ liftEffect curried >>= Promise.toAff
  where
  toExternal :: InternalEC2Response -> Array Instance
  toExternal response = do
    reservations <- response."Reservations"
    instances <- reservations."Instances"
    pure
      { id: InstanceId instances."InstanceId"
      , "type": InstanceType instances."InstanceType"
      }

  curried :: Effect (Promise InternalEC2Response)
  curried = describeInstancesImpl ec2

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

data Filter
  = Key (Array String)
  | ResourceId (Array String)
  | ResourceType (Array ResourceTypeName)
  | Tag String (Array String)
  | Value (Array String)

type InternalFilter
  = { "Name" :: String, "Values" :: Array String }

toInternalFilter :: Filter -> InternalFilter
toInternalFilter filter = case filter of
  Key values -> { "Name": "key", "Values": values }
  ResourceId values -> { "Name": "resource-id", "Values": values }
  ResourceType values -> { "Name": "resource-type", "Values": values <#> toInternalResourceTypeName }
  Tag key values -> { "Name": "tag:" <> key, "Values": values }
  Value values -> { "Name": "value:", "Values": values }

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

describeInstanceTypeAttribute :: EC2 -> Attribute -> InstanceId -> Aff (Either String (InstanceTypeAttribute ()))
describeInstanceTypeAttribute ec2 attribute instanceId =
  (toAffE $ curriedInstanceAttribute ec2 attribute instanceId)
    <#> parse
  where
  parse :: Json -> Either String (InstanceTypeAttribute ())
  parse =  decodeJson <#> lmap handleError
