{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import System.Directory ( doesFileExist )
import Control.Monad ( when, join, unless )
import System.Environment ( getArgs )
import SQL
import Data.ByteString.Lazy ( fromStrict  )
import Data.Maybe ( fromMaybe )
import RequestProcessors

app :: Application
app request respond

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
    
    -- POSTS

    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "add" = do
        -- processing posts endpoint, action list

        -- in this action server adds a new post
        putStrLn "Processing add post request"    
        (response_text, status) <- processAddPostRequest request
        respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "from_draft" = do
        -- processing posts endpoint, action from_draft

        -- in this action server publish post from draft
        putStrLn "Processing from draft request"    
        (response_text, status) <- processFromDraftRequest request
        respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "to_draft" = do
        -- processing posts endpoint, action from_draft

        -- in this action server publish post from draft
        putStrLn "Processing from draft request"    
        (response_text, status) <- processToDraftRequest request
        respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "add_tag" = do
        -- processing posts endpoint, action add_tag

        -- in this action server adds tag to a post
        putStrLn "Processing add tag to post request"
        (response_text, status) <- processAddTagToPostRequest request
        respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "remove_tag" = do
        -- processing posts endpoint, action remove_tag

        -- in this action server removes tag from a post
        putStrLn "Processing add tag to post request"
        (response_text, status) <- processRemoveTagFromPostRequest request
        respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "add_picture" = do
        -- processing posts endpoint, action add_picture

        -- in this action server adds tag to a post
        putStrLn "Processing add picture to post request"
        (response_text, status) <- processAddPictureToPostRequest request
        respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "remove_picture" = do
        -- processing posts endpoint, action remove_picture

        -- in this action server removes picture from a post
        putStrLn "Processing remove picture from post request"
        (response_text, status) <- processRemovePictureFromPostRequest request
        respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "list" = do
        -- processing posts endpoint, action remove_picture

        -- in this action server removes picture from a post
        putStrLn "Processing post listing request"
        response_text <- processListPostsRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" && action == "update" = do
        putStrLn "Processing update post request"
        (response_text, status) <- processUpdatePostRequest request
        respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- AUTH

    -- auth
    | length (pathInfo request) == 1 && last (pathInfo request) == "auth" && action == "auth" = do
        putStrLn "Processing auth request"
        (response_text, status) <- processAuthRequest request
        respond $ responseLBS
            status
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
        (response_text, status) <- processAddUsersRequest request
        respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- AUTHORS

    -- add
    | length (pathInfo request) == 1 && last (pathInfo request) == "authors" && action == "add" = do
        putStrLn "Processing add author request"    
        (response_text, status) <- processAddAuthorRequest request
        respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- delete
    | length (pathInfo request) == 1 && last (pathInfo request) == "authors" && action == "delete" = do
        putStrLn "Processing delete author request"    
        (response_text, status) <- processDeleteAuthorRequest request
        respond $ responseLBS
            status
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

    -- get one picture by id
    | length (pathInfo request) == 1 && last (pathInfo request) == "pictures" && action == "get_picture" = do
        putStrLn "Processing list pictures request"
        response_text <- processGetPictureRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "image/png")]
            (fromStrict response_text)

    -- CATEGORIES

    -- add
    | length (pathInfo request) == 1 && last (pathInfo request) == "categories" && action == "add" = do
        putStrLn "Processing add category request"    
        (response_text, status) <- processAddCategoryRequest request
        respond $ responseLBS
            status
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

    -- update
    | length (pathInfo request) == 1 && last (pathInfo request) == "categories" && action == "update" = do
        putStrLn "Processing update category request"
        (response_text, status) <- processUpdateCategoryRequest request
        respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

    -- TAGS

    -- add
    | length (pathInfo request) == 1 && last (pathInfo request) == "tags" && action == "add" = do
        putStrLn "Processing add tag request"
        response_text <- processAddTagRequest request
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromStrict response_text)

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
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            "Wrong path or action"
    where
        query = queryString request
        action = fromMaybe "" . join $ lookup "action" query

main :: IO ()
main = do
    -- get arguments
    args <- getArgs

    -- create database, if argument -create-db exists
    when ( "-create-db" `elem` args ) createNewsDb

    -- update database, if argument -update-db exists
    when ( elem "-update-db" args ) updateNewsDb

    -- check conf.cfg existense; create if it is not
    exist_conf_cfg <- doesFileExist "conf.cfg"
    unless exist_conf_cfg create_conf_cfg
    
    --  load configuration
    --(config, threadID) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]
    
    -- check news database existense
    news_db_exists <- check_news_db_existense
    putStrLn $ "news_db_exists = " ++ show news_db_exists

    -- run server, if all circumstanses are done
    when news_db_exists ( run 8080 app )

create_conf_cfg :: IO ()
create_conf_cfg = do
    -- write new conf file
    writeFile "conf.cfg" "import \"usr_config.cfg\""
