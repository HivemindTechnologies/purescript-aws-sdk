module AWS.EC2 where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps, Instance, InstanceId(..), InstanceType(..))
import Control.Promise (Promise, toAffE)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn1, Fn2, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))

foreign import data EC2 :: Type

foreign import newEC2 :: Foreign -> (Effect EC2)

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

data FilterName
  = Key
  | ResourceId
  | ResourceType ResourceTypeName
  | Tag String String
  | Value

type InternalFilter
  = { "Name" :: FilterName, "Values" :: Array String }

type Filter
  = { name :: FilterName, values :: Array String }

toInternalFilter :: Filter -> InternalFilter
toInternalFilter internalFilter = { "Name": internalFilter.name, "Values": internalFilter.values }

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
