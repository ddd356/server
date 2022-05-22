module Env where

import Data.ByteString (ByteString)
import SQL

data Env = Env
  { f_checkCredentials :: ByteString -> ByteString -> IO Bool,
    f_generateToken :: ByteString -> IO ByteString,
    f_addNewToken :: ByteString -> ByteString -> IO (),
    f_getUsersList :: Int -> Int -> IO SqlUsersList,
    f_totalNumberOfRowsInTable :: String -> IO Int,
    f_checkAdminRightsNew :: ByteString -> IO (Either String ()),
    f_loginNotExists :: ByteString -> IO (Either String ()),
    f_addUser :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> Bool -> Bool -> IO ByteString,
    -- TODO
    f_checkAdminRights :: ByteString -> IO Bool,
    f_checkLoginAndTokenAccordance :: ByteString -> ByteString -> IO Bool,
    f_addAuthor :: ByteString -> ByteString -> IO ByteString
    f_deleteAuthor :: ByteString -> IO ByteString
    f_addPicture :: ByteString -> IO ByteString
    f_getUsersList :: Int -> Int -> IO SqlPicturesList
    f_pictureIdExists :: Int -> IO (Either String ())
    f_getPicture :: Int -> IO ByteString
    f_canCreatePosts :: Token -> IO (Either String ())
    f_addPost :: ByteString -> Int -> Int -> ByteString -> Int -> IO ()
    f_checkPostAndTokenAccordance :: Int -> ByteString -> IO Bool
  }
