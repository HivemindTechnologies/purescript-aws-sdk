module AWS.Lambda where

import Prelude
import AWS.Core.Client (makeClientHelper, makeDefaultClient)
import AWS.Core.Types (Arn(..), DefaultClientPropsR, DefaultClientProps)
import Control.Promise (Promise, toAffE)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Simple.JSON (class WriteForeign, writeJSON)

type InternalLambdaParams
  = { "FunctionName" :: String
    , "Payload" :: String
    }

foreign import data Lambda :: Type

foreign import newLambda :: Foreign -> (Effect Lambda)

makeClient ::
  forall t4 t5 t6 t7 t8.
  RowToList t6 t5 =>
  FillableFields t5 () t6 =>
  Union
    t8
    t6
    DefaultClientPropsR =>
  RowToList t7 t4 => JustifiableFields t4 t7 () t8 => Record t7 -> Effect Lambda
makeClient r = ((makeDefaultClient r :: DefaultClientProps)) # makeClientHelper newLambda

foreign import invokeImpl :: forall output. Fn2 Lambda InternalLambdaParams (Effect (Promise output))

invoke :: forall input output. WriteForeign input => Lambda -> Arn -> input -> Aff output
invoke client (Arn arn) input = runFn2 invokeImpl client params # toAffE
  where
  params =
    { "FunctionName": arn
    , "Payload": writeJSON input
    }
