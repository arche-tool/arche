{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}

module Util.Storage
    ( StorageLink(..)
    , StorageLinkBuilder(..)
    , EncodedServiceAccountJson
    , loadStorageSigner
    , getPublicLink
    , getStorageObjectFullName
    , createObjectInsertion
    ) where

import Control.Lens                          ((&), (?~))
import Crypto.Hash
import Data.ByteArray                         (convert)
import Data.Text                              (Text)
import Data.Time.Clock.POSIX                  (getPOSIXTime)
import Network.Google.Auth.ApplicationDefault (defaultCredentialsPath, cloudSDKConfigPath)

import qualified Data.Aeson              as A
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL
import qualified Network.Google.Storage  as Storage

import Util.SignedUrl
import Type.Storage
import Type.Store (User(..))

type EncodedServiceAccountJson = T.Text

data StorageLinkBuilder
    = StorageLinkBuilder
    { readLinkBuilder  :: StorageObject -> Int -> IO StorageLink
    , writeLinkBuilder :: User -> StorageBucket -> Int -> IO StorageLink
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

loadReadLinkBuider :: ServiceAccount -> StorageObject -> Int -> IO StorageLink
loadReadLinkBuider account obj@StorageObject{objBucket} expSecs = do
    let oName = getStorageObjectFullName obj
        req = SignRequest               
            { httpVerb     = GET
            , bucketName   = T.unpack (bktName objBucket)
            , resourcePath = [T.unpack oName]
            , queryStrings = []
            , headers      = []
            , expires      = expSecs
            }
    url <- either fail pure =<< generateSignedURL account req
    return $ StorageLink
        { objectName = oName 
        , signedLink = T.decodeUtf8 url
        }

getUniqueObjectName :: User -> StorageBucket -> IO T.Text
getUniqueObjectName user bucket = do
    timestamp <- floor . (1e9 *) <$> getPOSIXTime
    let
        uid  = T.encodeUtf8 $ id_number user
        bkt  = T.encodeUtf8 $ bktName bucket
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
            , bucketName   = T.unpack (bktName bucket)
            , resourcePath = [T.unpack obj]
            , queryStrings = []
            , headers      = []
            , expires      = expSecs
            }
    url <- either fail pure =<< generateSignedURL account req
    return $ StorageLink
        { objectName = obj
        , signedLink = T.decodeUtf8 url
        }
            
getPublicLink :: StorageObject -> StoragePublicLink
getPublicLink obj@StorageObject{objBucket} = let
    fullName = getStorageObjectFullName obj
    in StoragePublicLink
      { publicName = fullName
      , publicLink = "https://storage.googleapis.com/" <> bktName objBucket <> "/" <> fullName
      }

getStorageObjectFullName :: StorageObject -> Text
getStorageObjectFullName StorageObject{objName, objExtension} =
    maybe objName ((objName <> ".") <>) objExtension

createObjectInsertion :: StorageObject -> Storage.ObjectsInsert
createObjectInsertion obj@StorageObject{objBucket} = let
    fullName = getStorageObjectFullName obj
    in Storage.objectsInsert (bktName objBucket) Storage.object' & Storage.oiName ?~ fullName
