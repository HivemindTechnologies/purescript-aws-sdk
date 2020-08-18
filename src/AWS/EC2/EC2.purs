module AWS.EC2 where

import Prelude
import AWS.Core (AccessKeyId(..), InstanceId(..), InstanceType(..), Instance, Region(..), SecretAccessKey(..), SessionToken(..))
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn1)
import Data.Maybe (Maybe)
import Data.Newtype (un)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

foreign import data EC2 :: Type

type InternalMakeClientParams
  = { region :: String
    , secretAccessKey :: String
    , accessKeyId :: String
    , sessionToken :: Nullable String
    }

foreign import makeClientImpl :: Fn1 InternalMakeClientParams (Effect EC2)

makeClient :: Region -> AccessKeyId -> SecretAccessKey -> Maybe SessionToken -> Effect EC2
makeClient r a s t =
  makeClientImpl
    { region: (un Region r)
    , secretAccessKey: (un SecretAccessKey s)
    , accessKeyId: (un AccessKeyId a)
    , sessionToken: toNullable $ map (un SessionToken) t
    }

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
