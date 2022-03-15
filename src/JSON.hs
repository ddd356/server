{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module JSON where

import Data.Aeson
import Data.ByteString ( ByteString )
import Data.ByteString.Lazy ( toStrict )
import GHC.Generics

data ResultUsersList = ResultUsersList {
    rul_limit   :: Int,
    rul_next    :: String,
    rul_prev    :: String,
    rul_users   :: [User]
} deriving (Generic, Show)

data User = User {
    usr_id          :: Int,
    urs_firstname   :: String,
    urs_lastname    :: String,
    urs_avatar      :: String,
    urs_create_date :: String,
    urs_admin       :: Bool
} deriving (Generic, Show)

instance ToJSON ResultUsersList where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }
        
instance ToJSON User where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

resultUsersList :: [[String]] -> ByteString
resultUsersList r = toStrict . encode $ rul where
    users = map \x -> 
    rul = ResultUsersList 0 "" "" [User 0 "" "" "" "" False]
