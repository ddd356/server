{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
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

app :: Application
app request respond

    --TEST

    | length (pathInfo request) == 1 && last (pathInfo request) == "test" = do
        -- for testing purposes

        ( params, files ) <- parseRequestBody lbsBackEnd request
        let file = toStrict . fileContent . snd . head $ files
        BS.writeFile "..\\..\\testfile1.png" file

        --body_lazy <- strictRequestBody  request
        --let body = toStrict body_lazy
        -- body = requestBody request
        --putStrLn $ "TEST: " ++ toString body
        -- BS.writeFile "..\\..\\testfile1.png" body
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            "Want to see a picture"
    
    -- POSTS

    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "add" = do
        -- processing posts endpoint, action list

        -- in this action server adds a new post
        putStrLn "Processing add post request"    
        response_text <- processAddPostRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "from_draft" = do
        -- processing posts endpoint, action from_draft

        -- in this action server publish post from draft
        putStrLn "Processing from draft request"    
        response_text <- processFromDraftRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "to_draft" = do
        -- processing posts endpoint, action from_draft

        -- in this action server publish post from draft
        putStrLn "Processing from draft request"    
        response_text <- processToDraftRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- AUTH

    -- auth
    | length (pathInfo request) == 1 && last (pathInfo request) == "auth" && action == "auth" = do
        putStrLn "Processing auth request"
        response_text <- processAuthRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- USERS

    -- list
    | length (pathInfo request) == 1 && last (pathInfo request) == "users" && action == "list" = do
        putStrLn "Processing list of users request"
        response_text <- processListUsersRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- add
    | length (pathInfo request) == 1 && last (pathInfo request) == "users" && action == "add" = do
        putStrLn "Processing add user request"
        response_text <- processAddUsersRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- AUTHORS

    -- add
    | length (pathInfo request) == 1 && last (pathInfo request) == "authors" && action == "add" = do
        putStrLn "Processing add author request"    
        response_text <- processAddAuthorRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- delete
    | length (pathInfo request) == 1 && last (pathInfo request) == "authors" && action == "delete" = do
        putStrLn "Processing delete author request"    
        response_text <- processDeleteAuthorRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- PICTURES

    -- add
    | length (pathInfo request) == 1 && last (pathInfo request) == "pictures" && action == "add" = do
        putStrLn "Processing add picture request"    
        response_text <- processAddPictureRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)
    -- list
    | length (pathInfo request) == 1 && last (pathInfo request) == "pictures" && action == "list" = do
        putStrLn "Processing list pictures request"
        response_text <- processListPicturesRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- CATEGORIES

    -- add
    | length (pathInfo request) == 1 && last (pathInfo request) == "categories" && action == "add" = do
        putStrLn "Processing add category request"    
        response_text <- processAddCategoryRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- list
    | length (pathInfo request) == 1 && last (pathInfo request) == "categories" && action == "list" = do
        putStrLn "Processing list category request"
        response_text <- processListCategoriesRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- OTHERWISE
        
    | otherwise = do
        -- print info message to command line
        putStrLn "I've done some IO here"
        putStrLn $ show (length $ pathInfo request)
        putStrLn $ show (last $ pathInfo request)
        putStrLn $ show (action)
        putStrLn $ show $ length (pathInfo request) == 1
        putStrLn $ show $ last (pathInfo request) == "pictures"
        putStrLn $ show $ action == "list"
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            "Wrong path or action"
    where
        query = queryString request
        action = fromMaybe "" . join $ lookup "action" query

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

main :: IO ()
main = do
    -- get arguments
    args <- getArgs

    -- create database, if argument -create-db exists
    when ( elem "-create-db" args ) createNewsDb

    -- update database, if argument -update-db exists
    when ( elem "-update-db" args ) updateNewsDb

    -- check conf.cfg existense; create if it is not
    exist_conf_cfg <- doesFileExist "conf.cfg"
    when (not exist_conf_cfg) create_conf_cfg
    
    --  load configuration
    (config, threadID) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]
    
    -- check news database existense
    news_db_exists <- check_news_db_existense
    putStrLn $ "news_db_exists = " ++ (show news_db_exists)

    -- run server, if all circumstanses are done
    when news_db_exists ( run 8080 app )

create_conf_cfg = do
    -- write new conf file
    writeFile "conf.cfg" "import \"usr_config.cfg\""
