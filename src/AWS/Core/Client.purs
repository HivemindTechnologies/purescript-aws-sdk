module AWS.Core.Client (makeDefaultClient, makeClientHelper) where

import AWS.Core.Types
import Effect (Effect)
import Foreign (Foreign)
import Justifill (justifill)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Prelude ((>>>))
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Simple.JSON (class WriteForeign, write)

makeClientHelper ::
  forall additionalProps client.
  WriteForeign (Record (BasicClientPropsR additionalProps)) =>
  (Foreign -> Effect client) -> Record (BasicClientPropsR additionalProps) -> Effect client
makeClientHelper newClient = write >>> newClient

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
  Record (BasicClientPropsR additionalProps)
makeDefaultClient r = ((justifill r) :: Record (BasicClientPropsR additionalProps))
