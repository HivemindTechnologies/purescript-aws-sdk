module AWS.Core.Util where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.DateTime (DateTime)
import Data.Either (Either, either)
import Data.Formatter.DateTime (formatDateTime)
import Effect.Exception (Error, error)

toIso8601Date :: DateTime -> Either String String
toIso8601Date d = formatDateTime "YYYY-MM-DD" d

raiseEither :: forall m r. MonadThrow Error m => Either String r -> m r
raiseEither = either (error >>> throwError) pure
