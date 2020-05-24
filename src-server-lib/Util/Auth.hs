{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Util.Auth
    ( userInfo
    , authHandler
    , authServerContext
    , mkOAuthClientID
    , AuthIDToken
    , OAuthClientID
    , TokenInfoV3
      ( iss
      , sub
      , azp
      , aud
      , iat
      , expire
      , email
      , email_verified
      , name
      , picture
      , given_name
      , family_name
      , locale
      )
    ) where

import Control.Monad                    (unless)
import Control.Monad.IO.Class           (liftIO)
import Data.Aeson                       (FromJSON(..), withObject, (.:), (.:?))
import Data.Proxy                       (Proxy (Proxy))
import Data.String                      (fromString)
import Data.Text                        (Text, strip, isSuffixOf, unpack)
import Data.Text.Encoding               (decodeUtf8)
import Data.Text.Read
import GHC.Generics                     (Generic)
import Network.HTTP.Client              (newManager)
import Network.HTTP.Client.TLS          (tlsManagerSettings)
import Network.Wai                      (Request, requestHeaders)
import Servant                          (throwError)
import Servant.API                      ((:>), Get, JSON, QueryParam)
import Servant.API.Experimental.Auth    (AuthProtect)
import Servant.Client
import Servant.Server                   (Context ((:.), EmptyContext), err401, errBody)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

import qualified Data.ByteString as BS
import qualified Data.Text       as T

type AuthIDToken = AuthProtect "google-id-token"
type instance AuthServerData AuthIDToken = TokenInfoV3

type IDToken = Text

newtype OAuthClientID = OAuthClientID { raw_client_id :: Text } deriving (Show, Eq, Generic)

instance FromJSON OAuthClientID where
  parseJSON = fmap OAuthClientID . parseJSON 

data TokenInfoV3
  = TokenInfoV3
  -- These six fields are included in all Google ID Tokens.
  { iss :: Text 
  , sub :: Text 
  , azp :: OAuthClientID 
  , aud :: Text 
  , iat :: Int 
  , expire :: Int 
  -- These seven fields are only included when the user has granted the "profile" and "email" OAuth scopes to the application.
  , email :: Maybe Text
  , email_verified :: Maybe Bool
  , name :: Maybe Text
  , picture :: Maybe Text
  , given_name :: Maybe Text
  , family_name :: Maybe Text
  , locale :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON TokenInfoV3 where
  parseJSON = withObject "TokenInfoV3" $ \o -> do
    iss <- o .: "iss"
    sub <- o .: "sub"
    azp <- o .: "azp"
    aud <- o .: "aud"
    iat <- o .: "iat" >>= readInt
    expire <- o .: "exp" >>= readInt
    email <- o .:? "email"
    email_verified <- o .:? "email_verified" >>= maybe (pure Nothing) (fmap Just . readBool)
    name <- o .:? "name"
    picture <- o .:? "picture"
    given_name <- o .:? "given_name"
    family_name <- o .:? "family_name"
    locale <- o .:? "locale"
    return TokenInfoV3{..}

readInt :: (Monad m, Integral a) => Text -> m a
readInt s = case decimal s of
  Left err     -> fail err
  Right (x, r)
    | T.length r == 0 -> return x
    | otherwise       -> fail "Not totaly a number."

readBool :: (Monad m) => Text -> m Bool
readBool s
  | norm == "true"  = return True 
  | norm == "false" = return True 
  | otherwise       = fail "Not a boolean value"
  where
    norm = T.toLower s

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

mkOAuthClientID :: Text -> OAuthClientID
mkOAuthClientID txt
  | suffix `isSuffixOf` txt = OAuthClientID txt
  | otherwise               = OAuthClientID (txt <> suffix) 
  where suffix = ".apps.googleusercontent.com"

authHandler :: OAuthClientID -> AuthHandler Request TokenInfoV3
authHandler expected_azp = mkAuthHandler $ \req -> do
  idToken   <- either throw401 return $ parseIDToken req
  tokenInfo <- either throw401 return =<< (liftIO $ userInfo idToken)
  unless (isEmailVerified tokenInfo) $
    throw401 "Account without verified email. Please, verify it first."
  unless (matchAZP tokenInfo) $
    throw401 ("Client ID mismatch: " <> (unpack . raw_client_id $ azp tokenInfo))
  unless (matchIssuer tokenInfo) $
    throw401 ("Unexpected issuer: " <> (unpack $ iss tokenInfo))
  return tokenInfo
  where
    matchAZP info = azp info == expected_azp 
    matchIssuer info = (iss info == "https://accounts.google.com") || (iss info == "accounts.google.com")
    isEmailVerified info = email_verified info == Just True
    throw401 msg = throwError $ err401 { errBody = fromString msg }

authServerContext :: OAuthClientID -> Context '[AuthHandler Request TokenInfoV3]
authServerContext expected_azp = authHandler expected_azp :. EmptyContext