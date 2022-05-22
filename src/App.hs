{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Monad (join, unless, when)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromMaybe)
import Env (Env (..))
import Log.Handle
import Log.Impl.BotLog as Log
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import RequestProcessors
import SQL
import System.Directory (doesFileExist)
import System.Environment (getArgs)

env :: Env
env =
  Env checkCredentials generateToken addNewToken getUsersList totalNumberOfRowsInTable checkAdminRightsNew loginNotExists addUser checkAdminRights checkLoginAndTokenAccordance addAuthor deleteAuthor addPicture getUsersList pictureIdExists getPicture canCreatePosts addPost

app :: Application
app request respond
  -- POSTS

  | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "add" = do
    Log.log DEBUG ("Request: " ++ show request)
    (response_text, status) <- runReaderT (processAddPostRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)
  | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "from_draft" = do
    Log.log DEBUG ("Request: " ++ show request)

    putStrLn "Processing from draft request"
    (response_text, status) <- runReaderT (processFromDraftRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)
  | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "to_draft" = do
    Log.log DEBUG ("Request: " ++ show request)

    (response_text, status) <- runReaderT (processToDraftRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)
  | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "add_tag" = do
    (response_text, status) <- runReaderT (processAddTagToPostRequest request) env

    Log.log DEBUG ("Request: " ++ show request)

    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)
  | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "remove_tag" = do
    Log.log DEBUG ("Request: " ++ show request)

    (response_text, status) <- runReaderT (processRemoveTagFromPostRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)
  | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "add_picture" = do
    Log.log DEBUG ("Request: " ++ show request)

    (response_text, status) <- runReaderT (processAddPictureToPostRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)
  | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "remove_picture" = do
    Log.log DEBUG ("Request: " ++ show request)

    (response_text, status) <- runReaderT (processRemovePictureFromPostRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)
  | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "list" = do
    Log.log DEBUG ("Request: " ++ show request)

    response_text <- runReaderT (processListPostsRequest request) env
    respond $
      responseLBS
        status200
        [("Content-Type", "text/plain")]
        (fromStrict response_text)
  | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "update" = do
    Log.log DEBUG ("Request: " ++ show request)

    (response_text, status) <- runReaderT (processUpdatePostRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)

  -- AUTH

  -- auth
  | length (pathInfo request) == 1 && last (pathInfo request) == "auth" && action == "auth" = do
    Log.log DEBUG ("Request: " ++ show request)

    (response_text, status) <- runReaderT (processAuthRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)

  -- USERS

  -- list
  | length (pathInfo request) == 1 && last (pathInfo request) == "users" && action == "list" = do
    Log.log DEBUG ("Request: " ++ show request)

    response_text <- runReaderT (processListUsersRequest request) env
    respond $
      responseLBS
        status200
        [("Content-Type", "text/plain")]
        (fromStrict response_text)

  -- add
  | length (pathInfo request) == 1 && last (pathInfo request) == "users" && action == "add" = do
    Log.log DEBUG ("Request: " ++ show request)

    (response_text, status) <- runReaderT (processAddUsersRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)

  -- AUTHORS

  -- add
  | length (pathInfo request) == 1 && last (pathInfo request) == "authors" && action == "add" = do
    Log.log DEBUG ("Request: " ++ show request)

    (response_text, status) <- runReaderT (processAddAuthorRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)

  -- delete
  | length (pathInfo request) == 1 && last (pathInfo request) == "authors" && action == "delete" = do
    Log.log DEBUG ("Request: " ++ show request)

    (response_text, status) <- runReaderT (processDeleteAuthorRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)

  -- PICTURES

  -- add
  | length (pathInfo request) == 1 && last (pathInfo request) == "pictures" && action == "add" = do
    Log.log DEBUG ("Request: " ++ show request)

    response_text <- runReaderT (processAddPictureRequest request) env
    respond $
      responseLBS
        status200
        [("Content-Type", "text/plain")]
        (fromStrict response_text)

  -- list
  | length (pathInfo request) == 1 && last (pathInfo request) == "pictures" && action == "list" = do
    Log.log DEBUG ("Request: " ++ show request)

    response_text <- runReaderT (processListPicturesRequest request) env
    respond $
      responseLBS
        status200
        [("Content-Type", "text/plain")]
        (fromStrict response_text)

  -- get one picture by id
  | length (pathInfo request) == 1 && last (pathInfo request) == "pictures" && action == "get_picture" = do
    Log.log DEBUG ("Request: " ++ show request)

    response_text <- runReaderT (processGetPictureRequest request) env
    respond $
      responseLBS
        status200
        [("Content-Type", "image/png")]
        (fromStrict response_text)

  -- CATEGORIES

  -- add
  | length (pathInfo request) == 1 && last (pathInfo request) == "categories" && action == "add" = do
    Log.log DEBUG ("Request: " ++ show request)

    (response_text, status) <- runReaderT (processAddCategoryRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)

  -- list
  | length (pathInfo request) == 1 && last (pathInfo request) == "categories" && action == "list" = do
    Log.log DEBUG ("Request: " ++ show request)

    response_text <- runReaderT (processListCategoriesRequest request) env
    respond $
      responseLBS
        status200
        [("Content-Type", "text/plain")]
        (fromStrict response_text)

  -- update
  | length (pathInfo request) == 1 && last (pathInfo request) == "categories" && action == "update" = do
    Log.log DEBUG ("Request: " ++ show request)

    (response_text, status) <- runReaderT (processUpdateCategoryRequest request) env
    respond $
      responseLBS
        status
        [("Content-Type", "text/plain")]
        (fromStrict response_text)

  -- TAGS

  -- add
  | length (pathInfo request) == 1 && last (pathInfo request) == "tags" && action == "add" = do
    Log.log DEBUG ("Request: " ++ show request)

    response_text <- runReaderT (processAddTagRequest request) env
    respond $
      responseLBS
        status200
        [("Content-Type", "text/plain")]
        (fromStrict response_text)

  --TEST

  --    | length (pathInfo request) == 1 && last (pathInfo request) == "test" = do
  --        -- for testing purposes
  --
  --        ( params, files ) <- parseRequestBody lbsBackEnd request
  --        let file = toStrict . fileContent . snd . head $ files
  --        BS.writeFile "..\\..\\testfile1.png" file
  --
  --        --body_lazy <- strictRequestBody  request
  --        --let body = toStrict body_lazy
  --        -- body = requestBody request
  --        --putStrLn $ "TEST: " ++ toString body
  --        -- BS.writeFile "..\\..\\testfile1.png" body
  --        respond $ responseLBS
  --            status200
  --            [("Content-Type", "text/plain")]
  --            "Want to see a picture"

  -- OTHERWISE

  | otherwise = do
    -- print info message to command line
    putStrLn "I've done some IO here"
    print (length $ pathInfo request)
    print (last $ pathInfo request)
    print action
    print (length (pathInfo request) == 1)
    print (last (pathInfo request) == "pictures")
    print (action == "list")
    respond $
      responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Wrong path or action"
  where
    query = queryString request
    action = fromMaybe "" . join $ lookup "action" query
