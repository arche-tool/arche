{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Util.Auth
    ( userInfo
    , authHandler
    , authServerContext
    , AuthIDToken
    , TokenInfoV3
      ( iss
      , sub
      , azp
      , aud
      , iat
      , exp
      , email
      , email_verified
      , name
      , picture
      , given_name
      , family_name
      , locale
      )
    ) where

import Control.Monad.IO.Class           (liftIO)
import Data.Aeson                       (FromJSON)
import Data.Proxy                       (Proxy (Proxy))
import Data.String                      (fromString)
import Data.Text                        (Text, strip)
import Data.Text.Encoding               (decodeUtf8)
import GHC.Generics                     (Generic)
import Network.HTTP.Client              (newManager)
import Network.HTTP.Client.TLS          (tlsManagerSettings)
import Network.Wai                      (Request, requestHeaders)
import Servant                          (throwError)
import Servant.API                      ((:>), Get, JSON, QueryParam)
import Servant.API.Experimental.Auth    (AuthProtect)
import Servant.Client
import Servant.Server                   (Context ((:.), EmptyContext), err401, errBody, Handler)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

import qualified Data.ByteString  as BS

type AuthIDToken = AuthProtect "google-id-token"
type instance AuthServerData AuthIDToken = TokenInfoV3

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

parseIDToken :: Request -> Either String IDToken
parseIDToken req = do
  authHeader <- maybeToEither "Missing authorization header" . lookup "Authorization" . requestHeaders $ req
  bearer <- maybeToEither "Invalid token format. Missing Bearer" $
    (strip . decodeUtf8) <$> BS.stripPrefix "Bearer" authHeader
  return bearer
  where
    maybeToEither e = maybe (Left e) Right

authHandler :: AuthHandler Request TokenInfoV3
authHandler = mkAuthHandler handler
  where
  throw401 msg = throwError $ err401 { errBody = fromString msg }
  verifyIdToken :: IDToken -> Handler TokenInfoV3
  verifyIdToken tk = (liftIO $ userInfo tk) >>= either throw401 return
  handler = either throw401 verifyIdToken . parseIDToken

authServerContext :: Context '[AuthHandler Request TokenInfoV3]
authServerContext = authHandler :. EmptyContext