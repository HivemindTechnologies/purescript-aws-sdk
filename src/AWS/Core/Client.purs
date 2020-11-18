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

makeClientHelper :: forall additionalProps client. WriteForeign (Record (PropsDefaultR additionalProps)) => 
  (Foreign -> Effect client) -> Record (PropsDefaultR additionalProps) -> Effect client
makeClientHelper newClient = write >>> newClient

makeDefaultClient ::
  forall additionalProps output input to toRL inputRL.
  Nub (PropsDefaultR additionalProps) (PropsDefaultR additionalProps) => 
  RowToList to toRL =>
  FillableFields toRL () to =>
  Union
    output
    to
    (PropsDefaultR additionalProps) =>
  RowToList input inputRL =>
  JustifiableFields inputRL input () output =>
  Record input ->
  Record (PropsDefaultR additionalProps)
makeDefaultClient r = ((justifill r) :: Record (PropsDefaultR additionalProps))
