module AWS.Core.Util where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.DateTime (DateTime)
import Data.Either (Either, either)
import Data.Formatter.DateTime (formatDateTime)
import Effect.Exception (Error, error)
import Data.Nullable (Nullable, toMaybe)
import Data.Unfoldable (fromMaybe)

toIso8601Date :: DateTime -> Either String String
toIso8601Date d = formatDateTime "YYYY-MM-DD" d

raiseEither :: forall m r. MonadThrow Error m => Either String r -> m r
raiseEither = either (error >>> throwError) pure

joinNullArr :: forall a. Nullable (Array a) -> Array a
joinNullArr = nullToArr >>> join

nullToArr :: forall a. Nullable a -> Array a
nullToArr = toMaybe >>> fromMaybe
