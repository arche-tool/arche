{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.GetSession where

import qualified Aws
import qualified Aws.Ses as SES
import qualified Data.UUID as UUID
import GHC.Generics
import Aws.Lambda
import Data.Aeson
import Data.Map
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Random (randomIO)

import Type.APIGateway
import Util.AWS

data Person = Person
  { name :: Text
  , email :: Text
  } deriving (Generic, FromJSON, ToJSON, Show)

newtype Token = Token {uuid :: UUID.UUID}

pageRootUrl :: Text
pageRootUrl = "https://arche-tool.github.io/arche/"

buildEmail :: SES.EmailAddress -> Token -> SES.SendRawEmail
buildEmail email token = let
  sender = SES.Sender "talktoedgar@gmail.com"
  msg = T.unlines [
    "From:" <> SES.senderAddress sender,
    "Subject: Start Arche new session",
    "",
    "Click on " <> pageRootUrl <> "index.html?token=" <> UUID.toText (uuid token)
    ]
  in SES.SendRawEmail {
    SES.srmDestinations = [email],
    SES.srmRawMessage = SES.RawMessage (encodeUtf8 msg),
    srmSource = Just sender
  }

handler :: Event Person -> Context -> IO (Either String (Response Text))
handler Event{..} _ = do
  token <- randomIO
  let
    mail = buildEmail (email body) (Token token)
    sesCfg :: SES.SesConfiguration Aws.NormalQuery
    sesCfg = SES.sesHttpsPost SES.sesUsEast1
  SES.SendRawEmailResponse {SES.srmrMessageId = rsp} <- runAWS sesCfg mail 
  pure $ Right Response
    { statusCode = 200
    , body = rsp
    , isBase64Encoded = False
    , headers = Data.Map.singleton "Content-Type" "application/json"
    }