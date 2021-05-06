module AWS.Core.Util where

import Prelude
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Tuple (Tuple, fst, snd)
import Data.Unfoldable (fromMaybe)
import Effect.Exception (Error, error)

toIso8601Date :: DateTime -> Either String String
toIso8601Date d = formatDateTime "YYYY-MM-DD" d

raiseEither :: forall m r. MonadThrow Error m => Either String r -> m r
raiseEither = either (error >>> throwError) pure

joinNullArr :: forall a. Nullable (Array a) -> Array a
joinNullArr = nullToArr >>> join

nullToArr :: forall a. Nullable a -> Array a
nullToArr = toMaybe >>> fromMaybe

catchAwsError :: forall m e s. MonadError e m => m (Either e s) -> m (Either e s)
catchAwsError = (flip catchError) (Left >>> pure)

handleError :: JsonDecodeError -> String
handleError = show

-- | Unfolds a monadic value with the given function.
-- | The generating function may put the result into the first element of the tuple
-- | and at the second element the element which needs to be passed on (if there is any of course).
-- | The first element of the result tuple will be accumlated with the other results and returned at the end.
-- | and wrapped in `ls`.
-- foldRecM :: forall m a b. MonadRec m => (b -> a -> m b) -> b -> Array a -> m b
unfoldrM1 ::
  forall m ls a b.
  Monad m =>
  Monoid (m a) =>
  Applicative ls =>
  Monoid (ls b) =>
  Maybe a ->
  (a -> m (Tuple b (Maybe a))) ->
  m (ls b)
unfoldrM1 initialMaybeToken func = do
  let
    rec (resSoFar :: ls b) maybeToken = do
      case maybeToken of
        Just newValue -> do
          res :: Tuple b (Maybe a) <- func newValue
          let
            newResSoFar :: ls b
            newResSoFar = resSoFar <> (pure $ fst res)
          rec newResSoFar $ snd res
        Nothing -> pure resSoFar
  rec mempty initialMaybeToken
