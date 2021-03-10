module AWS.Lambda
  ( Lambda
  , makeClient
  , invoke
  ) where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (Arn(..), DefaultClientProps)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (class EncodeJson, Json, encodeJson, stringify)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))

type InternalLambdaParams
  = { "FunctionName" :: String
    , "Payload" :: String
    }

foreign import data Lambda :: Type

foreign import newLambda :: Json -> (Effect Lambda)

makeClient ::
  forall r via.
  Justifiable { | r } { | via } =>
  Fillable { | via } DefaultClientProps =>
  { | r } ->
  Effect Lambda
makeClient r = makeClientHelper newLambda props
  where
  viaProxy :: Proxy { | via }
  viaProxy = Proxy

  props :: DefaultClientProps
  props = justifillVia viaProxy r

foreign import invokeImpl :: forall output. Fn2 Lambda InternalLambdaParams (Effect (Promise output))

invoke :: forall input output. EncodeJson input => Lambda -> Arn -> input -> Aff output
invoke client (Arn arn) input = runFn2 invokeImpl client params # toAffE
  where
  params =
    { "FunctionName": arn
    , "Payload": stringify (encodeJson input)
    }
