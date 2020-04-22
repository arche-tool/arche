{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric #-}

module Type.Store where

import qualified Network.Google.FireStore as FireStore
import Control.Lens ((&), (^.), (^?), (?~), (.~), ix, set)
import Data.Aeson   (ToJSON, FromJSON)
import Data.Text    (Text)
import GHC.Generics

import qualified Arche.Strategy.GomesGraph as GG
import qualified Arche.Strategy.ORFitAll   as OF

import Type.Storage (HashEBSD(..), HashOR(..), HashArche(..))
import Util.FireStore
import Util.OrphanInstances ()

-- ================ EBSD ================
data EBSD
    = EBSD
    { alias     :: Text
    , hashEBSD  :: HashEBSD
    , createdBy :: User
    } deriving (Show, Generic)

instance ToJSON EBSD

instance FromDocumentFields EBSD where
    fromDocFields fields = do
        _alias      <- getTextField fields "alias"
        _hash       <- getTextField fields "hash"
        _userFields <- getNestedDocumentField fields "user"
        _user       <- fromDocFields _userFields
        return $ EBSD
            { alias = _alias
            , hashEBSD = HashEBSD _hash
            , createdBy = _user
            }

instance ToDocumentFields EBSD where
    toDocFields ebsd = buildDocFields
        [ ("alias", toValue (alias ebsd))
        , ("hash",  toValue (hashEBSD ebsd))
        , ("user",  toValue (createdBy ebsd))
        ]

-- ================ OR ================
data OR
    = OR
    { hashOR   :: HashOR
    , cfgOR    :: OF.Cfg
    , resultOR :: OF.OREvaluation
    } deriving (Show, Generic)

instance ToJSON OR

-- ================ Arche ================
data Arche
    = Arche
    { hashArche :: HashArche
    , cfgArche  :: GG.Cfg
    } deriving (Show, Generic)

instance ToJSON Arche

-- ================ User ================
data User
    = User
    { email :: Text
    , name :: Maybe Text
    } deriving (Show, Generic)

instance Eq User where
    (==) (User e1 _) (User e2 _) = e1 == e2

instance ToJSON User

instance FromDocumentFields User where
    fromDocFields fields = do
        _email <- getTextField fields "email"
        _name  <- getMaybeTextField fields "name"
        return $ User
            { email = _email
            , name = _name
            }

instance ToDocumentFields User where
    toDocFields user = buildDocFields
        [ ("email", toValue (email user))
        , ("name",  toValueMaybe (name user))
        ]

instance ToDocValue User where
    toValue = toValue . toDocFields