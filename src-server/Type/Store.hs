{-# LANGUAGE OverloadedStrings   #-}
module Type.Store where

import qualified Network.Google.FireStore as FireStore
import Control.Lens                 ((&), (^.), (^?), (?~), (.~), ix, set)
import Data.Text                    (Text)

import Util.FireStore

-- ================ User ================
data User
    = User
    { email :: Text
    , name :: Maybe Text
    } deriving (Show)

instance FromDocument User where
    fromDoc doc = do
        fields <- getDocumentFields doc
        _email <- getTextField fields "email"
        _name  <- getMaybeTextField fields "name"
        return $ User
            { email = _email
            , name = _name
            }

instance ToDocument User where
    toDoc user = buildDoc
        [ ("email", toValue (email user))
        , ("name",  toValueMaybe (name user))
        ]
