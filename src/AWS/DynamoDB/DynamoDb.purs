module AWS.DynamoDb where

import Prelude
import AWS.Core.Client (makeClientHelper)
import AWS.Core.Types (DefaultClientProps)
import AWS.Core.Util (catchAwsError)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff, Error, catchError)
import Effect.Class (liftEffect)
import Justifill (justifillVia)
import Justifill.Fillable (class Fillable)
import Justifill.Justifiable (class Justifiable)
import Type.Proxy (Proxy(..))

foreign import data DynamoDbClient :: Type

foreign import newDynamoDbClient :: Json -> (Effect DynamoDbClient)

makeClient ::
  forall r via.
  Justifiable { | r } { | via } =>
  Fillable { | via } DefaultClientProps =>
  { | r } ->
  Effect DynamoDbClient
makeClient r = makeClientHelper newDynamoDbClient props
  where
  viaProxy :: Proxy { | via }
  viaProxy = Proxy

  props :: DefaultClientProps
  props = justifillVia viaProxy r

type GetItemInput input key
  = { "TableName" :: String, "Key" :: Record key | input }

type PutItemInput input item
  = { "TableName" :: String, "Item" :: Record item | input }

type InternalGetItemOutput output
  = { "Item" :: Nullable output }

type GetItemOutput output
  = { item :: Maybe output }

foreign import getItemImpl :: forall input key output. Fn2 DynamoDbClient (GetItemInput input key) (Effect (Promise (InternalGetItemOutput output)))

getItem :: forall input key output. DynamoDbClient -> (GetItemInput input key) -> Aff (Either Error (GetItemOutput output))
getItem client input = liftEffect (curried client input) >>= Promise.toAff <#> toExternal # catchAwsError
  where
  toExternal response =
    { item: Nullable.toMaybe response."Item" }
      # Right

  curried = runFn2 getItemImpl

foreign import putItemImpl :: forall input item. Fn2 DynamoDbClient (PutItemInput input item) (Effect (Promise Unit))

putItem :: forall input item. DynamoDbClient -> (PutItemInput input item) -> Aff (Maybe Error)
putItem client input = liftEffect (curried client input) >>= Promise.toAff <#> toExternal # (flip catchError) (Just >>> pure)
  where
  toExternal response = Nothing

  curried = runFn2 putItemImpl

type KeySchemaElement
  = { "AttributeName" :: String
    , "KeyType" :: String
    }

type AttributeDefinition
  = { "AttributeName" :: String
    , "AttributeType" :: String
    }

type CreateTableInput input
  = { "AttributeDefinitions" :: Array AttributeDefinition
    , "TableName" :: String
    , "KeySchema" :: Array KeySchemaElement
    | input
    }

foreign import createTableImpl :: forall input. Fn2 DynamoDbClient (CreateTableInput input) (Effect (Promise Unit))

createTable :: forall input. DynamoDbClient -> (CreateTableInput input) -> Aff (Maybe Error)
createTable client input = liftEffect (curried client input) >>= Promise.toAff <#> toExternal # (flip catchError) (Just >>> pure)
  where
  toExternal response = Nothing

  curried = runFn2 createTableImpl
