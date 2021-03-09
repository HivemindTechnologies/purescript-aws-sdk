module AWS.Core.Client (makeDefaultClient, makeClientHelper) where

import AWS.Core.Types
import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Effect (Effect)
import Justifill (justifill)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Prelude ((>>>))
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)

makeClientHelper ::
  forall additionalProps client.
  EncodeJson (BasicClientProps additionalProps) =>
  (Json -> Effect client) -> BasicClientProps additionalProps -> Effect client
makeClientHelper newClient = encodeJson >>> newClient

makeDefaultClient ::
  forall additionalProps output input to toRL inputRL.
  Nub (BasicClientPropsR additionalProps) (BasicClientPropsR additionalProps) =>
  RowToList to toRL =>
  FillableFields toRL () to =>
  Union
    output
    to
    (BasicClientPropsR additionalProps) =>
  RowToList input inputRL =>
  JustifiableFields inputRL input () output =>
  Record input ->
  BasicClientProps additionalProps
makeDefaultClient r = ((justifill r) :: BasicClientProps additionalProps)
