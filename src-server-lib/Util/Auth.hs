{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.Auth
    ( userInfo
    ) where

import Data.Aeson (FromJSON)
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.HTTP.Client     (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

type IDToken = Text

data TokenInfoV3
  = TokenInfoV3
  -- These six fields are included in all Google ID Tokens.
  { iss :: Text 
  , sub :: Text 
  , azp :: Text 
  , aud :: Text 
  , iat :: Text 
  , exp :: Text 
  -- These seven fields are only included when the user has granted the "profile" and "email" OAuth scopes to the application.
  , email :: Maybe Text
  , email_verified :: Maybe Text
  , name :: Maybe Text
  , picture :: Maybe Text
  , given_name :: Maybe Text
  , family_name :: Maybe Text
  , locale :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON TokenInfoV3

userInfo :: IDToken -> IO (Either String TokenInfoV3)
userInfo idToken = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "www.googleapis.com" 443 "")
  res <- runClientM (getTokenInfo $ Just idToken) env 
  return $ case res of
    Left err -> Left (show err)
    Right tk -> Right tk

type TokenInfoMethodGet =
  "oauth2" :> "v3" :> "tokeninfo" :>
    QueryParam "id_token" Text :>
      Get '[JSON] TokenInfoV3

tokeninfo :: Proxy TokenInfoMethodGet
tokeninfo = Proxy

getTokenInfo :: Maybe Text -> ClientM TokenInfoV3
getTokenInfo = client tokeninfo
