module AWS.CloudwatchLogs where

import Prelude
import AWS.Core (AccessKeyId(..), Region(..), SecretAccessKey(..), SessionToken(..))
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Newtype (un)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (ForeignError)
import Simple.JSON (readJSON)

foreign import data CloudwatchLogs :: Type

type InternalMakeClientParams
  = { region :: String
    , secretAccessKey :: String
    , accessKeyId :: String
    , sessionToken :: Nullable String
    }

foreign import makeClientImpl :: Fn1 InternalMakeClientParams (Effect CloudwatchLogs)

makeClient :: Region -> AccessKeyId -> SecretAccessKey -> Maybe SessionToken -> Effect CloudwatchLogs
makeClient r a s t =
  makeClientImpl
    { region: (un Region r)
    , secretAccessKey: (un SecretAccessKey s)
    , accessKeyId: (un AccessKeyId a)
    , sessionToken: toNullable $ map (un SessionToken) t
    }

type InternalGetDescribeLogGroupsOutput
  = { logGroups ::
        Array
          { arn :: String
          , creationTime :: Number
          , kmsKeyId :: String
          , logGroupName :: String
          , metricFilterCount :: Number
          , retentionInDays :: Number
          , storedBytes :: Number
          }
    , nextToken :: String
    }

foreign import getDescribeLogGroupsImpl :: Fn1 CloudwatchLogs (Effect (Promise String))

getDescribeLogGroups :: CloudwatchLogs -> Aff (Either String InternalGetDescribeLogGroupsOutput)
getDescribeLogGroups cw = liftEffect curried >>= Promise.toAff <#> parse
  where
  handleError :: NonEmptyList ForeignError -> String
  handleError = map show >>> fold

  parse :: String -> Either String InternalGetDescribeLogGroupsOutput
  parse = readJSON >>> bimap handleError identity

  curried :: Effect (Promise String)
  curried = runFn1 getDescribeLogGroupsImpl cw
