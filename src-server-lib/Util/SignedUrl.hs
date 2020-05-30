{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{--
Google Cloud Storage V4 Signing mechanism to sign URLs
https://cloud.google.com/storage/docs/access-control/signing-urls-manually
https://cloud.google.com/storage/docs/authentication/canonical-requests
https://cloud.google.com/storage/docs/authentication/signatures
--}
module Util.SignedUrl
    ( generateSignedURL
    , HttpVerb(..)
    , SignRequest(..)
    ) where

import Crypto.Hash.Algorithms
import Crypto.Hash                      (hash, Digest)
import Crypto.PubKey.RSA                (PrivateKey)
import Crypto.Store.PKCS8               (OptProtected(..), readKeyFileFromMemory)
import Crypto.PubKey.RSA.PKCS15         (signSafer)
import Data.ByteString                  (ByteString)
import Data.ByteString.Char8            (pack, unpack)
import Data.ByteArray                   (convert)
import Data.Char                        (toLower)
import Data.List                        (sortOn)
import Data.Time                        (UTCTime)
import Data.X509                        (PrivKey(..))

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
    , serviceAccountKey   :: String
    }

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
generateSignedURL serviceAccount signReq = let
    eKey = parsePrivateKey (serviceAccountKey serviceAccount)
    in case eKey of
        Left  err -> return $ Left err
        Right key -> do
            now <- T.getCurrentTime 
            let
                req = buildCanonicalReq signReq serviceAccount now
            eSign <- signCanonicalRequest req key
            return $ case eSign of
                Left err   -> Left err
                Right sign -> let
                    queries = canonicalQueryString req <> pack "&" <> renderQueryStrings [("X-Goog-Signature", unpack sign)]
                    hostname = pack $ "https://" <> bucketName signReq <> ".storage.googleapis.com"
                    url = hostname <> canonicalResource req <> pack "?" <> queries
                    in Right url 

buildCanonicalReq :: SignRequest -> ServiceAccount -> UTCTime -> CanonicalRequest
buildCanonicalReq req ServiceAccount{..} timestamp = let
    datestamp        = T.formatTime T.defaultTimeLocale "%Y%m%d" timestamp
    requestTimestamp = T.formatTime T.defaultTimeLocale "%Y%m%dT%H%M%SZ" timestamp
    credentialScope  = datestamp <> "/auto/storage/goog4_request" 
    credential       = serviceAccountEmail <> "/" <> credentialScope
    (headerStr, signedHeaderStr) = renderHeaders req
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
        slash = pack "/"

renderQueryStrings :: [(String, String)] -> ByteString
renderQueryStrings = HTTP.renderSimpleQuery False . map (\(k, v) -> (pack k, pack v)) . sortOn fst

renderHeaders :: SignRequest -> (ByteString, ByteString)
renderHeaders req = let
    renderHeader (k, v) = pack $ k <> ":" <> v
    orderedHeaders = sortOn fst (headers req)
    headerStr = BS.map toLower . BS.unlines . map renderHeader $ orderedHeaders
    signedHeaderStr = BS.map toLower . BS.intercalate (pack ";") . map (pack . fst) $ orderedHeaders
    in (headerStr, signedHeaderStr)

parsePrivateKey :: String -> Either String PrivateKey
parsePrivateKey raw = let
    pk = readKeyFileFromMemory . pack $ raw
    in case pk of
        [Unprotected (PrivKeyRSA rsa)] -> Right rsa
        [Unprotected _]                -> Left "Provided private key is not of RSA type"
        [Protected   _]                -> Left "Provided key is protected by password"
        []                             -> Left "No key was found"
        _                              -> Left "More than one key was found"

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
        , maybe (pack "UNSIGNED-PAYLOAD") id canonicalPayload
        ]
    in str

stringToSign :: CanonicalRequest -> ByteString
stringToSign req = unlinesNoTrailing
    [ pack "GOOG4-RSA-SHA256"
    , pack (time req)
    , pack (scope req)
    , sha256sum (encodeCanonicalRequest req)
    ]

signCanonicalRequest :: CanonicalRequest -> PrivateKey -> IO (Either String ByteString)
signCanonicalRequest signature key = do
    let enc = stringToSign signature
    sign <- signSafer (Just SHA256) key enc 
    return $ either (Left . show) (Right . toHex) sign
