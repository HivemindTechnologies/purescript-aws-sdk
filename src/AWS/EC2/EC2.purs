module AWS.EC2 (makeClient, describeInstances) where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps, Instance, InstanceId(..), InstanceType(..))
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn1)
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
