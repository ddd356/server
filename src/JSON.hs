{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module JSON where

import SQL ( SqlUsersList, SqlPicturesList, SqlCategoriesList )
import Data.Aeson
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS ( append )
import Data.ByteString.Lazy ( toStrict, fromStrict )
import Data.ByteString.UTF8 ( toString )
import Data.ByteString.Char8 ( readInt )
import Data.String ( fromString )
import GHC.Generics
import Data.Maybe ( fromJust )

type PaginationData = (Total, Limit, From)
type Total = Int
type Limit = Int
type From = Int

type Token = ByteString

data RequestResult = RequestResult {
    res_result :: String,
    res_error_description :: String
} deriving (Generic, Show)

data ResultCategoriesList = ResultCategoriesList {
    rcl_limit       :: Int,
    rcl_next        :: String,
    rcl_prev        :: String,
    rcl_categories  :: Value
} deriving (Generic, Show)

data Category = Category {
    cat_id      :: Int,
    cat_name    :: String,
    cat_subcategories  :: [Category]
} deriving (Generic, Show)

data ResultUsersList = ResultUsersList {
    rul_limit   :: Int,
    rul_next    :: String,
    rul_prev    :: String,
    rul_users   :: [User]
} deriving (Generic, Show)

data User = User {
    usr_id                  :: Int,
    usr_firstname           :: String,
    usr_lastname            :: String,
    usr_login               :: String,
    usr_avatar              :: String,
    usr_create_date         :: String,
    usr_admin               :: Bool,
    usr_can_create_posts    :: Bool
} deriving (Generic, Show)

data ResultPicturesList = ResultPicturesList {
    rpl_limit       :: Int,
    rpl_next        :: String,
    rpl_prev        :: String,
    rpl_pictures    :: [Picture]
} deriving (Generic, Show)

data Picture = Picture {
    pic_id      :: Int,
    pic_picture :: String
} deriving (Generic, Show)

data ResultPostsList = ResultPostsList {
    pol_limit       :: Int,
    pol_next        :: String,
    pol_prev        :: String,
    pol_posts       :: Value
} deriving (Generic, Show)

instance ToJSON RequestResult where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance ToJSON ResultUsersList where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }
        
instance ToJSON User where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance ToJSON ResultPicturesList where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance ToJSON Picture where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance ToJSON ResultCategoriesList where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance ToJSON Category where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance ToJSON ResultPostsList where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

resultUsersList :: SqlUsersList -> PaginationData -> ByteString
resultUsersList l (total, limit, from) = toStrict . encode $ rul where
    users = map (\(id, firstname, lastname, login, avatar, create_date, admin, can_create_posts) -> User id firstname lastname login (toString avatar) (show create_date) admin can_create_posts) l
    next = if total > from + limit then ( "/users?" ++ "action=list" ++ "&from=" ++ (show $ from + limit) ++ "&limit=" ++ (show limit)  ) else ""
    prev = if from > 0 then ( "users?" ++ "action=list" ++ "&from" ++ (show $ max (from - limit) 0 ) ++ "&limit=" ++ (show limit) ) else ""
    rul = ResultUsersList limit next prev users

resultRequest :: String -> String -> ByteString
resultRequest res description = toStrict . encode $ RequestResult res description

resultPicturesList :: SqlPicturesList -> PaginationData -> ByteString
resultPicturesList l (total, limit, from) = toStrict . encode $ rpl where
    pictures = map (\ (id, picture) -> Picture id (toString picture)) l
    next = if total > from + limit then ( "/pictures?" ++ "action=list" ++ "&from=" ++ (show $ from + limit) ++ "&limit=" ++ (show limit)  ) else ""
    prev = if from > 0 then ( "/pictures?" ++ "action=list" ++ "&from=" ++ (show $ max (from - limit) 0 ) ++ "&limit=" ++ (show limit) ) else ""
    rpl = ResultPicturesList limit next prev pictures

resultCategoriesList :: ByteString -> PaginationData -> ByteString
resultCategoriesList categories (total, limit, from) = toStrict . encode $ rcl where
    next = if total > from + limit then ( "/categories?" ++ "action=list" ++ "&from=" ++ (show $ from + limit) ++ "&limit=" ++ (show limit) ) else ""
    prev = if from > 0 then ( "/categories?" ++ "action=list" ++ "&from=" ++ (show $ max (from - limit) 0 ) ++ "&limit=" ++ (show limit) ) else ""
    val = fromJust . decode . fromStrict $ categories :: Value
    rcl = ResultCategoriesList limit next prev val

resultPostsList :: ByteString -> PaginationData -> ByteString 
resultPostsList posts (total, limit, from) = toStrict . encode $ rpl where
    next = if total > from + limit then ( "/posts?" ++ "action=list" ++ "&from=" ++ (show $ from + limit) ++ "&limit=" ++ (show limit)  ) else ""
    prev = if from > 0 then ( "posts?" ++ "action=list" ++ "&from" ++ (show $ max (from - limit) 0 ) ++ "&limit=" ++ (show limit) ) else ""
    val = fromJust . decode . fromStrict $ posts :: Value
    rpl = ResultPostsList limit next prev val
