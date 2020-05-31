{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{--
Google Cloud Storage V4 Signing mechanism to sign URLs
https://cloud.google.com/storage/docs/access-control/signing-urls-manually
https://cloud.google.com/storage/docs/authentication/canonical-requests
https://cloud.google.com/storage/docs/authentication/signatures
--}
module Util.SignedUrl
    ( generateSignedURL
    , HttpVerb(..)
    , ServiceAccount(..)
    , SignRequest(..)
    , toHex
    ) where

import Crypto.Hash.Algorithms
import Crypto.Hash                      (hash, Digest)
import Crypto.PubKey.RSA                (PrivateKey)
import Crypto.Store.PKCS8               (OptProtected(..), readKeyFileFromMemory)
import Crypto.PubKey.RSA.PKCS15         (signSafer)
import Data.Aeson                       (FromJSON, (.:))
import Data.ByteString                  (ByteString)
import Data.ByteString.Char8            (pack, unpack)
import Data.ByteArray                   (convert)
import Data.Char                        (toLower)
import Data.List                        (sortOn)
import Data.Text.Encoding               (encodeUtf8)
import Data.Time                        (UTCTime)
import Data.X509                        (PrivKey(..))

import qualified Data.Aeson              as A
import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Time               as T
import qualified Network.HTTP.Types      as HTTP

data HttpVerb
    = DELETE
    | GET
    | HEAD
    | POST
    | PUT
    deriving Show

data ServiceAccount
    = ServiceAccount
    { serviceAccountEmail :: String
    , serviceAccountKey   :: PrivateKey
    }

instance FromJSON ServiceAccount where
    parseJSON = A.withObject "service_account" $ \obj -> do
        pk <- (parsePrivateKey . encodeUtf8) <$> obj .: "private_key"
        ServiceAccount
            <$> obj .: "client_email"
            <*> pk

data SignRequest    
    = SignRequest               
    { httpVerb       :: HttpVerb
    , bucketName     :: String
    , resourcePath   :: [String]
    , queryStrings   :: [(String, String)]
    , headers        :: [(String, String)]
    , expires        :: Int
    }
   
data CanonicalRequest    
    = CanonicalRequest               
    { canonicalHttpVerb      :: ByteString
    , canonicalResource      :: ByteString
    , canonicalQueryString   :: ByteString
    , canonicalHeaders       :: ByteString
    , canonicalSignedHeaders :: ByteString
    , canonicalPayload       :: Maybe ByteString
    , date                   :: String
    , time                   :: String
    , scope                  :: String
    }

generateSignedURL :: ServiceAccount -> SignRequest -> IO (Either String ByteString)
generateSignedURL account signReq = do
    now <- T.getCurrentTime 
    let
        req = buildCanonicalReq signReq account now
    eSign <- signCanonicalRequest req (serviceAccountKey account)
    return $ case eSign of
        Left err   -> Left err
        Right sign -> let
            queries = canonicalQueryString req <> "&" <> renderQueryStrings [("X-Goog-Signature", unpack sign)]
            hostname = pack $ "https://" <> getHostname signReq 
            url = hostname <> canonicalResource req <> "?" <> queries
            in Right url 

getHostname :: SignRequest -> String
getHostname req = bucketName req <> ".storage.googleapis.com"

buildCanonicalReq :: SignRequest -> ServiceAccount -> UTCTime -> CanonicalRequest
buildCanonicalReq req ServiceAccount{..} timestamp = let
    datestamp        = T.formatTime T.defaultTimeLocale "%Y%m%d" timestamp
    requestTimestamp = T.formatTime T.defaultTimeLocale "%Y%m%dT%H%M%SZ" timestamp
    credentialScope  = datestamp <> "/auto/storage/goog4_request" 
    credential       = serviceAccountEmail <> "/" <> credentialScope
    hostHeader       = ("Host", getHostname req)
    (headerStr, signedHeaderStr) = renderHeaders $ headers req <> [hostHeader]
    extraQS =
        [ ("X-Goog-Algorithm",    "GOOG4-RSA-SHA256")
        , ("X-Goog-Credential",    credential)
        , ("X-Goog-Date",          requestTimestamp)
        , ("X-Goog-Expires",       show (expires req))
        , ("X-Goog-SignedHeaders", unpack signedHeaderStr)
        ]
    in CanonicalRequest
        { canonicalHttpVerb      = renderVerb req 
        , canonicalResource      = sanitizeResourcePath req
        , canonicalQueryString   = renderQueryStrings (queryStrings req <> extraQS)
        , canonicalHeaders       = headerStr
        , canonicalSignedHeaders = signedHeaderStr
        , canonicalPayload       = Nothing 
        , date                   = datestamp 
        , time                   = requestTimestamp
        , scope                  = credentialScope
        }

renderVerb :: SignRequest -> ByteString
renderVerb = pack. show . httpVerb

sanitizeResourcePath :: SignRequest -> ByteString
sanitizeResourcePath req = slash <> assemble (resourcePath req)
    where
        assemble = BS.intercalate slash . map (HTTP.urlEncode True . pack)
        slash = "/"

renderQueryStrings :: [(String, String)] -> ByteString
renderQueryStrings = HTTP.renderSimpleQuery False . map (\(k, v) -> (pack k, pack v)) . sortOn fst

renderHeaders :: [(String, String)] -> (ByteString, ByteString)
renderHeaders hs = let
    renderHeader (k, v) = pack $ k <> ":" <> v
    orderedHeaders = sortOn fst hs
    headerStr = BS.map toLower . BS.unlines . map renderHeader $ orderedHeaders
    signedHeaderStr = BS.map toLower . BS.intercalate ";" . map (pack . fst) $ orderedHeaders
    in (headerStr, signedHeaderStr)

parsePrivateKey :: (Monad m) => ByteString -> m PrivateKey
parsePrivateKey raw = let
    pk = readKeyFileFromMemory raw
    in case pk of
        [Unprotected (PrivKeyRSA rsa)] -> pure rsa
        [Unprotected _]                -> fail "Provided private key is not of RSA type"
        [Protected   _]                -> fail "Provided key is protected by password"
        []                             -> fail "No key was found"
        _                              -> fail "More than one key was found"

toHex :: ByteString -> ByteString
toHex = BL.toStrict . BB.toLazyByteString . BB.byteStringHex

sha256sum :: ByteString -> ByteString
sha256sum x = let 
    digest = hash x :: Digest SHA256
    in toHex . convert $ digest

unlinesNoTrailing :: [ByteString] -> ByteString
unlinesNoTrailing = BS.intercalate newline
    where newline = BS.singleton '\n'

encodeCanonicalRequest :: CanonicalRequest -> ByteString
encodeCanonicalRequest CanonicalRequest{..} = let
    str = unlinesNoTrailing $ 
        [ canonicalHttpVerb
        , canonicalResource
        , canonicalQueryString
        , canonicalHeaders
        , canonicalSignedHeaders
        , maybe "UNSIGNED-PAYLOAD" id canonicalPayload
        ]
    in str

stringToSign :: CanonicalRequest -> ByteString
stringToSign req = unlinesNoTrailing
    [ "GOOG4-RSA-SHA256"
    , pack (time req)
    , pack (scope req)
    , sha256sum (encodeCanonicalRequest req)
    ]

signCanonicalRequest :: CanonicalRequest -> PrivateKey -> IO (Either String ByteString)
signCanonicalRequest signature key = do
    let enc = stringToSign signature
    sign <- signSafer (Just SHA256) key enc 
    return $ either (Left . show) (Right . toHex) sign
