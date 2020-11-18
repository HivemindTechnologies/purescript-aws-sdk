module AWS.EC2 where

import Prelude

import AWS.Core.Client (makeClientHelper, makeDefaultClient)
import AWS.Core.Types (Instance, InstanceId(..), InstanceType(..), PropsDefaultR)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn1)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)

foreign import data EC2 :: Type

foreign import newEC2 :: Foreign -> (Effect EC2)

type PropsR = PropsDefaultR ()
type Props = Record PropsR

makeClient :: forall t4 t5 t6 t7 t8.
  RowToList t6 t5 => FillableFields t5 () t6 => Union t8 t6
                                                  PropsR
                                                 => RowToList t7 t4 => JustifiableFields t4 t7 () t8 => Record t7 -> Effect EC2
makeClient r = ((makeDefaultClient r:: Props)) # makeClientHelper newEC2

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
