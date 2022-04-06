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
import Data.ByteString ( ByteString, unpack )
import qualified Data.ByteString as BS (writeFile)
import Data.ByteString.Lazy ( fromStrict, toStrict )
import Data.ByteString.UTF8 ( toString )
import Data.ByteString.Char8 ( readInt )
import Data.Maybe ( fromMaybe, isNothing, fromJust )
import Control.Monad ( join )
import Data.String ( fromString )
import Network.Wai.Parse ( parseRequestBody, lbsBackEnd, FileInfo(..) )
import Database.PostgreSQL.Simple.Time ( parseLocalTime )
import Data.Char ( toLower )

defaultLimit = 50
defaultFrom = 0

getParam p query = case lookup p $ query of
    Just x -> x
    _ -> Nothing

check_all_parameters_nothing ps =  if all isNothing ps then Left "All parameters empty" else Right ()

-- AUTH

processAuthRequest :: Request -> IO (ByteString, Status)
processAuthRequest request = do

    let query = queryString request
    let login = fromMaybe "" . join $ lookup "login" query
    let password = fromMaybe "" . join $ lookup "password" query
   
    credentials_correct  <- checkCredentials login password
    token <- generateToken login

    case credentials_correct of
        True -> do
            addNewToken login token
            return (token, status200)
        _ -> return ("Authorization fails", status401)

-- USERS

processListUsersRequest :: Request -> IO ByteString
processListUsersRequest request = do
    -- get token from request
    let from = fst . fromMaybe (defaultFrom, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int
    let limit = min defaultLimit . min defaultLimit . fst . fromMaybe (defaultLimit, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int

    res <- getUsersList from limit
    total <- totalNumberOfRowsInTable "users"
    return $ resultUsersList res (total, limit, from)

processAddUsersRequest :: Request -> IO (ByteString, Status)
processAddUsersRequest request = do

    -- get params from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request 
    let firstname = fromMaybe "" . join $ lookup "firstname" $ queryString request
    let lastname = fromMaybe "" . join $ lookup "lastname" $ queryString request
    res <- parseRequestBody lbsBackEnd request
    let avatar = case res of
           ([],[]) -> ""
           _ -> toStrict . fileContent . snd . head . snd $ res
    let login = fromMaybe "" . join $ lookup "login" $ queryString request
    let password = fromMaybe "" . join $ lookup "password" $ queryString request
    let admin = read . toString . fromMaybe "False" . join $ lookup "admin" $ queryString request
    let can_create_posts = read . toString . fromMaybe "False" . join $ lookup "can_create_posts" $ queryString request

    -- check credentials for admin rights
    check_token <- checkAdminRightsNew token
    check_login <- loginNotExists login

    let check = (\_ -> check_token) () >>= (\_ -> check_login)

    case check of
        Left "Access denied" -> return ("", status404)
        Left x -> return (resultRequest "error" x, status400)
        _ -> do 
            addUser firstname lastname avatar login password admin can_create_posts
            return (resultRequest "ok" "User added", status200)

-- AUTHORS

processAddAuthorRequest :: Request -> IO (ByteString, Status)
processAddAuthorRequest request = do
    
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let login = fromMaybe "" . join $ lookup "login" $ queryString request
    let description = fromMaybe "" . join $ lookup "description" $ queryString request

    admin_rights <- checkAdminRights token
    token_accords <- checkLoginAndTokenAccordance login token

    let credentials_correct = admin_rights || token_accords

    case credentials_correct of
        True -> do
            addAuthor login description
            return (resultRequest "ok" "Author added", status200)
        _ -> return ("Access denied", status401)

processDeleteAuthorRequest :: Request -> IO (ByteString, Status)
processDeleteAuthorRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let login = fromMaybe "" . join $ lookup "login" $ queryString request

    admin_rights <- checkAdminRights token
    token_accords <- checkLoginAndTokenAccordance login token

    let credentials_correct = admin_rights || token_accords

    case credentials_correct of
        True -> do
            deleteAuthor login
            return (resultRequest "ok" "Author deleted", status200)
        _ -> return ("Access denied", status401)

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
    let limit = min defaultLimit . fst . fromMaybe (2, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int

    res <- getPicturesList from limit
    total <- totalNumberOfRowsInTable "pictures"
    return $ resultPicturesList res (total, limit, from)

processGetPictureRequest :: Request -> IO ByteString
processGetPictureRequest request = do

    let query = queryString request

    let id = getParam "id" query

    check_id <- case id of
            Nothing -> return $ Left "id must be set"
            Just x -> case readInt x of
                Nothing -> return $ Left "id incorrect"
                _ -> pictureIdExists . bytestringToInt . fromJust $ id

    let check = (\_ -> check_id) ()

    case check of
        Left x -> return $ resultRequest "error" x
        _ -> do
            res <- getPicture . bytestringToInt . fromJust $ id
            return res

-- CATEGORIES

-- add
processAddCategoryRequest :: Request -> IO (ByteString, Status)
processAddCategoryRequest request = do
    -- get params from request
    let name = fromMaybe "" . join $ lookup "name" $ queryString request
    let parent_id = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "parent_id" $ queryString request :: Int
    let token = fromMaybe "" . join $ lookup "token" $ queryString request

    -- check credentials for admin rights
    credentials_correct <- checkAdminRights token

    case credentials_correct of
        True -> do
            addCategory name parent_id
            return (resultRequest "ok" "Category added", status200)
        _ -> return ("Access denied", status401)

-- list
processListCategoriesRequest :: Request -> IO ByteString
processListCategoriesRequest request = do
    -- get params from request
    let from = fst . fromMaybe (defaultFrom, "") . readInt . fromMaybe "" . join $ lookup "from" $ queryString request :: Int
    let limit = min defaultLimit . fst . fromMaybe (defaultLimit, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int

    let pp = PaginationParameters limit from
    res <- getCategoriesList pp
    total <- totalNumberOfRowsInTable "categories"

    return $ resultCategoriesList res (total, limit, from)

-- update
processUpdateCategoryRequest :: Request -> IO (ByteString, Status)
processUpdateCategoryRequest request = do
    -- get params from request
    let query = queryString request

    let id = getParam "id" query
    let name = getParam "name" query
    let pid = getParam "pid" query
    let token = getParam "token" query

    -- checks
    credentials_correct <- case token of
        Nothing -> return $ Left "Access denied"
        Just x -> checkAdminRightsNew x

    check_id_int <- case id of
            Nothing -> return $ Left "id must be set"
            Just x -> case readInt x of
                Nothing -> return $ Left "id incorrect"
                _ -> categoryIdExists $ bytestringToInt . fromJust $ id

    let check_pid_int_or_null_or_empty = case pid of
            Nothing -> Right ()
            Just "null" -> Right ()
            Just x -> case readInt x of
                Nothing -> Left "pid incorrect"
                _ -> Right ()


    let check = (\_ -> credentials_correct) () >>= (\_ -> check_all_parameters_nothing [name, pid]) >>= (\_ -> check_id_int) >>= (\_ -> check_pid_int_or_null_or_empty)

    let vals = []
            ++ (if isNothing name then [] else [("name", fromJust name)])
            ++ (if isNothing pid then [] else [("pid", fromJust pid)])

    case check of
        Left "Access denied" -> return ("Access denied", status401)
        Left x -> return (resultRequest "error" x, status400)
        _ -> do
            res <- updateCategory (bytestringToInt . fromJust $ id) vals
            return (resultRequest "ok" (toString res), status200)

-- POSTS

processAddPostRequest :: Request -> IO (ByteString, Status)
processAddPostRequest request = do
    -- get params from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    --let login = fromMaybe "" . join $ lookup "login" $ queryString request
    let author = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "author" $ queryString request :: Int
    let shortName = fromMaybe "" . join $ lookup "shortname" $ queryString request
    let createDate = fromMaybe "" . join $ lookup "create_date" $ queryString request
    let category = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "category" $ queryString request :: Int
    let text = fromMaybe "" . join $ lookup "text" $ queryString request
    let mainPicture = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "main_picture" $ queryString request :: Int

    can_create_posts <- canCreatePosts token

    case can_create_posts of
        Left _ -> return ("Access denied", status401)
        _ -> do 
                addPost shortName author createDate category text mainPicture
                return ("Post added", status200)

processFromDraftRequest :: Request -> IO (ByteString, Status)
processFromDraftRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int

    admin_rights <- checkAdminRights token
    token_accords <- checkPostAndTokenAccordance postID token

    let credentials_correct = admin_rights || token_accords

    case credentials_correct of
        True -> do
                fromDraft postID
                return ("Post published", status200)
        _ -> return ("Access denied", status401)

processToDraftRequest :: Request -> IO (ByteString, Status)
processToDraftRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int

    admin_rights <- checkAdminRights token
    token_accords <- checkPostAndTokenAccordance postID token

    let credentials_correct = admin_rights || token_accords

    case credentials_correct of
        True -> do
                toDraft postID
                return ("Post moved to drafts", status200)
        _ -> return ("Access denied", status401)

processAddTagToPostRequest :: Request -> IO (ByteString, Status)
processAddTagToPostRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int
    let tagID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "tag_id" $ queryString request :: Int

    admin_rights <- checkAdminRights token
    token_accords <- checkPostAndTokenAccordance postID token

    let credentials_correct = admin_rights || token_accords

    case credentials_correct of
        True -> do
                addTagToPost postID tagID
                return ("Tag added to post", status200)
        _ -> return ("Access denied", status401)

processRemoveTagFromPostRequest :: Request -> IO (ByteString, Status)
processRemoveTagFromPostRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int
    let tagID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "tag_id" $ queryString request :: Int

    admin_rights <- checkAdminRights token
    token_accords <- checkPostAndTokenAccordance postID token

    let credentials_correct = admin_rights || token_accords

    case credentials_correct of
        True -> do
                removeTagFromPost postID tagID
                return ("Tag removed from post", status200)
        _ -> return ("Access denied", status401)

processAddPictureToPostRequest :: Request -> IO (ByteString, Status)
processAddPictureToPostRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int
    let pictureID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "picture_id" $ queryString request :: Int

    admin_rights <- checkAdminRights token
    token_accords <- checkPostAndTokenAccordance postID token

    let credentials_correct = admin_rights || token_accords

    case credentials_correct of
        True -> do
                addPictureToPost postID pictureID
                return ("Picture added to post", status200)
        _ -> return ("Access denied", status401)

processRemovePictureFromPostRequest :: Request -> IO (ByteString, Status)
processRemovePictureFromPostRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int
    let pictureID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "picture_id" $ queryString request :: Int

    admin_rights <- checkAdminRights token
    token_accords <- checkPostAndTokenAccordance postID token

    let credentials_correct = admin_rights || token_accords

    case credentials_correct of
        True -> do
                removePictureFromPost postID pictureID
                return ("Picture removed from post", status200)
        _ -> return ("Access denied", status401)

processListPostsRequest :: Request -> IO ByteString
processListPostsRequest request = do
    -- get params from request
    
    -- credentials
    let token = fromMaybe "" . join $ lookup "token" $ queryString request

    -- draft (special filter)
    let showDraft = case fromMaybe "" . join $ lookup "draft" $ queryString request of
            "true" -> True
            _ -> False

    -- filters
    let createdAt = case parseLocalTime . fromMaybe "" . join $ lookup "created_at" $ queryString request of 
            Right x -> Just x
            _ -> Nothing
    let createdUntil = case parseLocalTime . fromMaybe "" . join $ lookup "created_until" $ queryString request of
            Right x -> Just x
            _ -> Nothing
    let createdSince = case parseLocalTime . fromMaybe "" . join $ lookup "created_since" $ queryString request of
            Right x -> Just x
            _ -> Nothing
    let author = fromMaybe "" . join $ lookup "author" $ queryString request
    let categoryID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "category_id" $ queryString request :: Int
    let nameContains = fromMaybe "" . join $ lookup "name_contains" $ queryString request
    let textContains = fromMaybe "" . join $ lookup "text_contains" $ queryString request
    let search = fromMaybe "" . join $ lookup "search" $ queryString request

    -- sorting
    let sortBy = case fromMaybe "" . join $ lookup "sort_by" $ queryString request of
            "date" -> SortByDate
            "author" -> SortByAuthor
            "category" -> SortByCategory
            "pictures_quantity" -> SortByPicturesQuantity
            _ -> SortById

    -- pagination
    let limit = min defaultLimit . fst . fromMaybe (defaultLimit, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int
    let from = fst . fromMaybe (defaultFrom, "") . readInt . fromMaybe "" . join $ lookup "from" $ queryString request :: Int

    let fp = FilteringParameters createdAt createdUntil createdSince author categoryID nameContains textContains search
    let sp = SortingParameters sortBy
    let pp = PaginationParameters limit from

    res <- getPostsList fp sp pp token showDraft
    total <- totalNumberOfPosts fp
    return $ resultPostsList res (total, limit, from)

processUpdatePostRequest :: Request -> IO (ByteString, Status)
processUpdatePostRequest request = do
    -- get params from request
    let query = queryString request

    let id = getParam "id" query
    let token = getParam "token" query
    let shortName = getParam "shortname" query
    let category = getParam "category" query
    let text = getParam "text" query
    let mainPicture = getParam "main_picture" query

    -- checks
    let check_id = case id of
            Nothing -> Left "id must be set"
            Just x -> case readInt x of
                Nothing -> Left "id incorrect"
                _ -> Right ()

    check_category <- case category of
            Nothing -> return $ Right ()
            Just x -> case readInt x of
                Nothing -> return $ Left "category incorrect"
                _ -> categoryIdExists $ bytestringToInt . fromJust $ category

    check_main_picture <- case mainPicture of
            Nothing -> return $ Right ()
            Just x -> case readInt x of
                Nothing -> return $ Left "main_picture incorrect"
                _ -> pictureIdExists $ bytestringToInt . fromJust $ mainPicture

    credentials_correct <- case token of
            Nothing -> return $ Left "Access denied"
            Just x -> checkPostAndTokenAccordanceNew (bytestringToInt . fromJust $ id) x

    let check = (\_ -> check_all_parameters_nothing [shortName, category, text, mainPicture]) () >>= (\_ -> check_id) >>= (\_ -> check_category) >>= (\_ -> check_main_picture) >>= (\_ -> credentials_correct)

    let vals = []
            ++ (if isNothing shortName then [] else [("short_name", fromJust shortName)])
            ++ (if isNothing category then [] else [("category", fromJust category)])
            ++ (if isNothing text then [] else [("text", fromJust text)])
            ++ (if isNothing mainPicture then [] else [("main_picture", fromJust mainPicture)])

    case check of
        Left "Access denied" -> return ("Access denied", status401)
        Left x -> return (resultRequest "error" x, status400)
        _ -> do
            res <- updatePost (bytestringToInt . fromJust $ id) vals
            return $ (resultRequest "ok" (toString res), status200)

-- TAGS

processAddTagRequest :: Request -> IO ByteString
processAddTagRequest request = do
    -- get params from request
    let name = fromMaybe "" . join $ lookup "name" $ queryString request

    addTag name
