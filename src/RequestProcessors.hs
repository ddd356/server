{-# LANGUAGE OverloadedStrings #-}
module RequestProcessors where

import JSON
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Data.Configurator as Cfg ( autoReload, autoConfig, Worth(..), require )
import qualified Data.Configurator.Types as Cfg ( AutoConfig(..) )
import qualified Data.Text as T ( Text )
import qualified Data.Text.IO as T ( putStrLn )
import Control.Concurrent ( threadDelay )
import System.Directory ( doesFileExist )
import Control.Monad ( when )
import System.Environment ( getArgs )
--import SQL.Handle
import SQL
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS (writeFile)
import Data.ByteString.Lazy ( fromStrict, toStrict )
import Data.ByteString.UTF8 ( toString )
import Data.ByteString.Char8 ( readInt )
import Data.Maybe ( fromMaybe )
import Control.Monad ( join )
import Data.String ( fromString )
import Network.Wai.Parse ( parseRequestBody, lbsBackEnd, FileInfo(..) )

-- AUTH

processAuthRequest :: Request -> IO ByteString
processAuthRequest request = do
    -- lookup for parameters in request
    let query = queryString request
    let login = fromMaybe "" . join $ lookup "login" query
    let password = fromMaybe "" . join $ lookup "password" query
   
    -- check credentials
    credentials_correct  <- checkCredentials login password

    -- generating new unique token 
    token <- generateToken login

    -- saving new generated token into database
    addNewToken login token
    return token

-- USERS

processListUsersRequest :: Request -> IO ByteString
processListUsersRequest request = do
    -- get token from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let from = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int
    let limit = fst . fromMaybe (2, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int
    
    -- check credentials for admin rights
    credentials_correct <- checkAdminRights token

    -- in case of credential correctness return user list; otherwise return error message
    case credentials_correct of
        True -> do
            res <- getUsersList from limit
            total <- totalNumberOfRowsInTable "users"
            return $ resultUsersList res (total, limit, from) token
        _ -> return $ resultRequest "error" "You have no admin rights to watch users list"

processAddUsersRequest :: Request -> IO ByteString
processAddUsersRequest request = do

    -- get params from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request 
    let firstname = fromMaybe "" . join $ lookup "firstname" $ queryString request
    let lastname = fromMaybe "" . join $ lookup "lastname" $ queryString request
    res <- parseRequestBody lbsBackEnd request
    let avatar = toStrict . fileContent . snd . head . snd $ res
    let login = fromMaybe "" . join $ lookup "login" $ queryString request
    let password = fromMaybe "" . join $ lookup "password" $ queryString request
    let admin = read . toString . fromMaybe "False" . join $ lookup "admin" $ queryString request

    -- check credentials for admin rights
    credentials_correct <- checkAdminRights token

    -- in case of credential correctness add a new user; otherwise return error message
    case credentials_correct of
        True -> do 
            addUser firstname lastname avatar login password admin
            return $ resultRequest "ok" ""
        _ -> return $ resultRequest "error" "You have no admin rights to watch users list"

-- AUTHORS

processAddAuthorRequest :: Request -> IO ByteString
processAddAuthorRequest request = do
    
    -- get params from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let login = fromMaybe "" . join $ lookup "login" $ queryString request
    let description = fromMaybe "" . join $ lookup "description" $ queryString request

    -- check admin rights
    admin_rights <- checkAdminRights token
    
    -- check accordance of login and token
    token_accords <- checkLoginAndTokenAccordance login token

    -- credentials may be correct if user have admin rights or token accords to login
    let credentials_correct = admin_rights || token_accords

    -- in case of credential correctness add a new author; otherwise return error message
    case credentials_correct of
        True -> do
            addAuthor login description
            return $ resultRequest "ok" ""
        _ -> return $ resultRequest "error" "You have no permission to add author for this user"

processDeleteAuthorRequest :: Request -> IO ByteString
processDeleteAuthorRequest request = do

    -- get params from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let login = fromMaybe "" . join $ lookup "login" $ queryString request

    -- check admin rights
    admin_rights <- checkAdminRights token

    -- check accordance of login and token
    token_accords <- checkLoginAndTokenAccordance login token

    -- credentials may be correct if user have admin rights or token accords to login
    let credentials_correct = admin_rights || token_accords

    -- in case of credential correctness add a new author; otherwise return error message
    case credentials_correct of
        True -> do
            deleteAuthor login
            return $ resultRequest "ok" ""
        _ -> return $ resultRequest "error" "You have no permission to delete author for this user"

-- PICTURES

processAddPictureRequest :: Request -> IO ByteString
processAddPictureRequest request = do
    -- get params from request
    res <- parseRequestBody lbsBackEnd request
    let picture = toStrict . fileContent . snd . head . snd $ res

    -- add a new picture
    addPicture picture
    return $ resultRequest "ok" ""

processListPicturesRequest :: Request -> IO ByteString
processListPicturesRequest request = do
    -- get params from request
    let from = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int
    let limit = fst . fromMaybe (2, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int

    res <- getPicturesList from limit
    total <- totalNumberOfRowsInTable "pictures"
    return $ resultPicturesList res (total, limit, from)

-- CATEGORIES

-- add
processAddCategoryRequest :: Request -> IO ByteString
processAddCategoryRequest request = do
    -- get params from request
    let name = fromMaybe "" . join $ lookup "name" $ queryString request
    let parent_id = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "parent_id" $ queryString request :: Int

    addCategory name parent_id
    return $ resultRequest "ok" ""

-- list
processListCategoriesRequest :: Request -> IO ByteString
processListCategoriesRequest request = do
    -- get params from request
    let from = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int
    let limit = fst . fromMaybe (2, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int

    res <- getCategoriesList
    --total <- totalNumberOfRowsInTable "categories"
    --return $ resultCategoriesList res (total, limit, from)
    return res

-- POSTS

processAddPostRequest :: Request -> IO ByteString
processAddPostRequest request = do
    -- get params from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let login = fromMaybe "" . join $ lookup "login" $ queryString request
    let author = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "author" $ queryString request :: Int
    let shortName = fromMaybe "" . join $ lookup "shortname" $ queryString request
    let createDate = fromMaybe "" . join $ lookup "create_date" $ queryString request
    let category = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "category" $ queryString request :: Int
    let text = fromMaybe "" . join $ lookup "text" $ queryString request
    let mainPicture = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "main_picture" $ queryString request :: Int

    -- check admin rights
    admin_rights <- checkAdminRights token
    
    -- check accordance of login and token
    token_accords <- checkLoginAndTokenAccordance login token

    -- credentials may be correct if user have admin rights or token accords to login
    let credentials_correct = admin_rights || token_accords

    -- in case of credential correctness add a new author; otherwise return error message
    case credentials_correct of
        True -> addPost login shortName author createDate category text mainPicture
        _ -> return "You have no permission to delete author for this user"

processFromDraftRequest :: Request -> IO ByteString
processFromDraftRequest request = do
    -- get params from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int

    -- check admin rights
    admin_rights <- checkAdminRights token

    -- check accordance of post and token
    token_accords <- checkPostAndTokenAccordance postID token

    -- credentials may be correct if user have admin rights or token accords to login
    let credentials_correct = admin_rights || token_accords

    -- in case of credential correctness add a new author; otherwise return error message
    case credentials_correct of
        True -> fromDraft postID
        _ -> return "You have no permission to delete author for this user"

processToDraftRequest :: Request -> IO ByteString
processToDraftRequest request = do
    -- get params from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int

    -- check admin rights
    admin_rights <- checkAdminRights token

    -- check accordance of post and token
    token_accords <- checkPostAndTokenAccordance postID token

    -- credentials may be correct if user have admin rights or token accords to login
    let credentials_correct = admin_rights || token_accords

    -- in case of credential correctness add a new author; otherwise return error message
    case credentials_correct of
        True -> toDraft postID
        _ -> return "You have no permission to delete author for this user"

processAddTagToPostRequest :: Request -> IO ByteString
processAddTagToPostRequest request = do
    -- get params from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int
    let tagID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "tag_id" $ queryString request :: Int

    -- check admin rights
    admin_rights <- checkAdminRights token

    -- check accordance of post and token
    token_accords <- checkPostAndTokenAccordance postID token

    -- credentials may be correct if user have admin rights or token accords to login
    let credentials_correct = admin_rights || token_accords

    -- in case of credential correctness add a new author; otherwise return error message
    case credentials_correct of
        True -> addTagToPost postID tagID
        _ -> return "You have no permission to delete author for this user"

processRemoveTagFromPostRequest :: Request -> IO ByteString
processRemoveTagFromPostRequest request = do
    -- get params from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int
    let tagID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "tag_id" $ queryString request :: Int

    -- check admin rights
    admin_rights <- checkAdminRights token

    -- check accordance of post and token
    token_accords <- checkPostAndTokenAccordance postID token

    -- credentials may be correct if user have admin rights or token accords to login
    let credentials_correct = admin_rights || token_accords

    -- in case of credential correctness add a new author; otherwise return error message
    case credentials_correct of
        True -> removeTagFromPost postID tagID
        _ -> return "You have no permission to delete author for this user"


-- TAGS

processAddTagRequest :: Request -> IO ByteString
processAddTagRequest request = do
    -- get params from request
    let name = fromMaybe "" . join $ lookup "name" $ queryString request

    addTag name
