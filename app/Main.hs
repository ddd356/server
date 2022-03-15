{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
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
import Data.Maybe ( fromMaybe )
import Control.Monad ( join )
import Data.String ( fromString )
import Network.Wai.Parse ( parseRequestBody, lbsBackEnd, FileInfo(..) )

        

app :: Application
app request respond
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
    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" = do
        -- processing posts endpoint
        putStrLn "I've done some IO here"
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            "Here must be returned posts"
    | length (pathInfo request) == 1 && last (pathInfo request) == "auth" && action == "auth" = do
        -- processing auth endpoint, action auth

        -- in this action server will check credentials and return new token or error message
        putStrLn "Processing auth request"
        response_text <- processAuthRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "users" && action == "list" = do
        -- processing users endpoint, action list

        -- in this action server returns list of users, but only if querying user have admin rights
        putStrLn "Processing list of users request"
        response_text <- processListUsersRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "users" && action == "add" = do
        -- processing users endpoint, action add

        -- in this action server adds new user to database
        putStrLn "Processing add user request"
        response_text <- processAddUsersRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "authors" && action == "add" = do
        -- processing authors endpoint, action add

        -- in this action server adds new author for user
        putStrLn "Processing add author request"    
        response_text <- processAddAuthorRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "authors" && action == "delete" = do
        -- processing authors endpoint, action delete

        -- in this action server deletes author for user
        putStrLn "Processing delete author request"    
        response_text <- processDeleteAuthorRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "add" = do
        -- processing posts endpoint, action list

        -- in this action server adds a new post
        putStrLn "Processing add post request"    
        response_text <- processAddPostRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- | length (pathInfo request) == 1 && last (pathInfo request) == "pictures" && action == "add" = do
        -- processing pictures endpoint, action add

        -- in this action server adds a new picture
        putStrLn "Processing add picture request"    
        response_text <- processAddPictureRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- | length (pathInfo request) == 1 && last (pathInfo request) == "pictures" && action == "list" = do
        -- processing pictures endpoint, action list
        -- in this action server returns list of pictures with id
        putStrLn "Processing list pictures request"
        response_text <- processListPicturesRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)
        
    | otherwise = do
        -- print info message to command line
        putStrLn "I've done some IO here"
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            "Wrong path or action"
    where
        query = queryString request
        action = fromMaybe "" . join $ lookup "action" query

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

processListUsersRequest :: Request -> IO ByteString
processListUsersRequest request = do
    -- get token from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    
    -- check credentials for admin rights
    credentials_correct <- checkAdminRights token

    -- in case of credential correctness return user list; otherwise return error message
    case credentials_correct of
        True -> getUsersList
        _ -> return "You have no admin rights to watch users list"

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
        True -> addUser firstname lastname avatar login password admin
        _ -> return "You have no admin rights to watch users list"

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
        True -> addAuthor login description
        _ -> return "You have no permission to add author for this user"

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
        True -> deleteAuthor login
        _ -> return "You have no permission to delete author for this user"

processAddPictureRequest :: Request -> IO ByteString
processAddPictureRequest request = do
    -- get params from request
    res <- parseRequestBody lbsBackEnd request
    let picture = toStrict . fileContent . snd . head . snd $ res

    -- add a new picture
    addPicture picture

processListPicturesRequest :: Request -> IO ByteString
processListPicturesRequest request = do
    -- get params from request

processAddPostRequest :: Request -> IO ByteString
processAddPostRequest request = do
    -- get params from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let login = fromMaybe "" . join $ lookup "login" $ queryString request
    let shortName = fromMaybe "" . join $ lookup "shortname" $ queryString request
    let createDate = fromMaybe "" . join $ lookup "createdate" $ queryString request
    let category = fromMaybe "" . join $ lookup "category" $ queryString request
    let text = fromMaybe "" . join $ lookup "text" $ queryString request
    res <- parseRequestBody lbsBackEnd request
    let mainPicture = toStrict . fileContent . snd . head . snd $ res

    -- check admin rights
    admin_rights <- checkAdminRights token
    
    -- check accordance of login and token
    token_accords <- checkLoginAndTokenAccordance login token

    -- credentials may be correct if user have admin rights or token accords to login
    let credentials_correct = admin_rights || token_accords

    -- in case of credential correctness add a new author; otherwise return error message
    case credentials_correct of
        True -> addPost login shortName createDate category text mainPicture
        _ -> return "You have no permission to delete author for this user"

main :: IO ()
main = do
    -- get arguments
    args <- getArgs

    -- create database, if argument -create-db exists
    when ( elem "-create-db" args ) createNewsDb

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
