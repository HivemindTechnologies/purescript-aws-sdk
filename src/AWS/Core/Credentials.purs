module AWS.Core.Credentials where

import AWS.Core.Types (Credentials)
import Effect (Effect)

foreign import newSharedIniFileCredentials :: { profile :: String } -> Effect Credentials
