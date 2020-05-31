module Util.Storage
    ( StorageLink(..)
    , StorageLinkBuilder(..)
    , EncodedServiceAccountJson
    , Object
    , loadStorageSigner
    ) where

import Data.ByteArray                         (convert)
import Data.Text                              (Text)
import Data.Time.Clock.POSIX                  (getPOSIXTime)
import Crypto.Hash
import Network.Google.Auth.ApplicationDefault (defaultCredentialsPath, cloudSDKConfigPath)

import qualified Data.Aeson              as A
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL

import Util.SignedUrl
import Type.Storage
import Type.Store (User(..))

type EncodedServiceAccountJson = T.Text
type Object = String

data StorageLinkBuilder
    = StorageLinkBuilder
    { readLinkBuilder  :: Object -> StorageBucket -> Int -> IO StorageLink
    , writeLinkBuilder :: User   -> StorageBucket -> Int -> IO StorageLink
    }

loadServiceAccoutFromDefaultLocation :: IO ServiceAccount
loadServiceAccoutFromDefaultLocation = do
    file <- maybe cloudSDKConfigPath pure =<< defaultCredentialsPath
    either fail pure =<< A.eitherDecodeFileStrict' file

loadServiceAccoutFromBase64 :: Text -> Either String ServiceAccount
loadServiceAccoutFromBase64 x = do
    dec <- B64.decode . T.encodeUtf8 $ x
    A.eitherDecodeStrict' dec

loadStorageSigner :: Maybe EncodedServiceAccountJson -> IO StorageLinkBuilder 
loadStorageSigner mAccount = do
    account <- maybe
        loadServiceAccoutFromDefaultLocation
        (either fail pure . loadServiceAccoutFromBase64)
        mAccount
    return $ StorageLinkBuilder
        { readLinkBuilder  = loadReadLinkBuider account
        , writeLinkBuilder = loadWriteLinkBuider account
        }

loadReadLinkBuider :: ServiceAccount -> Object -> StorageBucket -> Int -> IO StorageLink
loadReadLinkBuider account obj bucket expSecs = do
    let req = SignRequest               
            { httpVerb     = GET
            , bucketName   = T.unpack (bktText bucket)
            , resourcePath = [obj]
            , queryStrings = []
            , headers      = []
            , expires      = expSecs
            }
    url <- either fail pure =<< generateSignedURL account req
    return $ StorageLink
        { objectPath = T.pack obj 
        , signedLink = T.decodeUtf8 url
        }

getUniqueObjectName :: User -> StorageBucket -> IO T.Text
getUniqueObjectName user bucket = do
    timestamp <- floor . (1e9 *) <$> getPOSIXTime
    let
        uid  = T.encodeUtf8 $ id_number user
        bkt  = T.encodeUtf8 $ bktText bucket
        salt = BL.toStrict . BB.toLazyByteString . BB.int64HexFixed $ timestamp
        dig  = hashFinalize $ hashInitWith SHA1 `hashUpdate` uid `hashUpdate` bkt `hashUpdate` salt 
        hex  = T.decodeUtf8 . toHex . convert $ dig
    return hex

loadWriteLinkBuider :: ServiceAccount -> User -> StorageBucket -> Int -> IO StorageLink
loadWriteLinkBuider account user bucket expSecs = do
    obj <- getUniqueObjectName user bucket
    let 
        req = SignRequest               
            { httpVerb     = PUT
            , bucketName   = T.unpack (bktText bucket)
            , resourcePath = [T.unpack obj]
            , queryStrings = []
            , headers      = []
            , expires      = expSecs
            }
    url <- either fail pure =<< generateSignedURL account req
    return $ StorageLink
        { objectPath = obj
        , signedLink = T.decodeUtf8 url
        }
            
