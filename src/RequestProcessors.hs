{-# LANGUAGE OverloadedStrings #-}
module RequestProcessors where

import Log.Impl.BotLog as Log
import Log.Handle
import JSON (resultUsersList, resultRequest, resultPicturesList, resultCategoriesList, resultPostsList )
import Network.Wai ( Request, queryString )
import Network.HTTP.Types ( Status, status200, status401, status400, status404 )
import SQL ( checkCredentials, generateToken, addNewToken, getUsersList, totalNumberOfRowsInTable, checkAdminRightsNew, loginNotExists, addUser, checkAdminRights, checkLoginAndTokenAccordance, addAuthor, addTag, bytestringToInt, updatePost, checkPostAndTokenAccordance, checkPostAndTokenAccordanceNew, pictureIdExists, categoryIdExists, totalNumberOfPosts, getPostsList, PaginationParameters(..), SortingParameters(..), FilteringParameters(..), SortingField(..), removePictureFromPost, addPictureToPost, removeTagFromPost, addTagToPost, toDraft, fromDraft, addPost, canCreatePosts, updateCategory, getCategoriesList, addCategory, getPicture, getPicturesList, addPicture, deleteAuthor, canCreatePosts, updateCategory, getCategoriesList, addCategory)
import Data.ByteString ( ByteString )
import Data.ByteString.Lazy ( toStrict )
import Data.ByteString.UTF8 ( toString )
import Data.ByteString.Char8 ( readInt )
import Data.Maybe ( fromMaybe, isJust, isNothing, fromJust )
import Control.Monad ( join )
import Network.Wai.Parse ( parseRequestBody, lbsBackEnd, FileInfo(..) )
import Database.PostgreSQL.Simple.Time ( parseLocalTime )

defaultLimit, defaultFrom :: Int
defaultLimit = 50
defaultFrom = 0

getParam :: ByteString -> [(ByteString, Maybe ByteString)] -> Maybe ByteString
getParam p query = case lookup p query of
    Just x -> x
    _ -> Nothing

checkAllParametersNothing :: [Maybe a] -> Either String ()
checkAllParametersNothing ps =  if all isNothing ps then Left "All parameters empty" else Right ()

-- AUTH

processAuthRequest :: Request -> IO (ByteString, Status)
processAuthRequest request = do

    let query = queryString request
    let login = fromMaybe "" . join $ lookup "login" query
    let password = fromMaybe "" . join $ lookup "password" query
   
    credentials_correct  <- checkCredentials login password

    Log.log DEBUG ("Credentials_correct=" ++ show credentials_correct)

    token <- generateToken login

    if credentials_correct then 
        (do
            addNewToken login token
            return (token, status200))
    else
        return ("Authorization fails", status401)

-- USERS

processListUsersRequest :: Request -> IO ByteString
processListUsersRequest request = do
    -- get token from request
    let frm = fst . fromMaybe (defaultFrom, "") . readInt . fromMaybe "" . join $ lookup "from" $ queryString request :: Int
    let lmt = min defaultLimit . fst . fromMaybe (defaultLimit, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int

    res <- getUsersList frm lmt
    total <- totalNumberOfRowsInTable "users"
    return $ resultUsersList res (total, lmt, frm)

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

    let check = check_token >> check_login

    case check of
        Left "Access denied" -> return ("", status404)
        Left x -> return (resultRequest "error" x, status400)
        _ -> do 
            _ <- addUser firstname lastname avatar login password admin can_create_posts
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

    if credentials_correct then
        (do _ <- addAuthor login description
            return (resultRequest "ok" "Author added", status200))
    else
        return ("Access denied", status401)

processDeleteAuthorRequest :: Request -> IO (ByteString, Status)
processDeleteAuthorRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let login = fromMaybe "" . join $ lookup "login" $ queryString request

    admin_rights <- checkAdminRights token
    token_accords <- checkLoginAndTokenAccordance login token

    let credentials_correct = admin_rights || token_accords

    if credentials_correct then
        (do _ <- deleteAuthor login
            return (resultRequest "ok" "Author deleted", status200))
    else
        return ("Access denied", status401)

-- PICTURES

processAddPictureRequest :: Request -> IO ByteString
processAddPictureRequest request = do
    -- get params from request
    res <- parseRequestBody lbsBackEnd request
    let picture = toStrict . fileContent . snd . head . snd $ res

    -- add a new picture
    _ <- addPicture picture
    return $ resultRequest "ok" ""

processListPicturesRequest :: Request -> IO ByteString
processListPicturesRequest request = do
    -- get params from request
    let frm = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "from" $ queryString request :: Int
    let lmt = min defaultLimit . fst . fromMaybe (2, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int

    res <- getPicturesList frm lmt
    total <- totalNumberOfRowsInTable "pictures"
    return $ resultPicturesList res (total, lmt, frm)

processGetPictureRequest :: Request -> IO ByteString
processGetPictureRequest request = do

    let query = queryString request

    let picture_id = getParam "id" query

    check_id <- case picture_id of
            Nothing -> return $ Left "id must be set"
            Just x -> case readInt x of
                Nothing -> return $ Left "id incorrect"
                _ -> pictureIdExists . bytestringToInt . fromJust $ picture_id

    let check = check_id

    case check of
        Left x -> return $ resultRequest "error" x
        _ -> do
            getPicture . bytestringToInt . fromJust $ picture_id

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

    if credentials_correct then
        (do _ <- addCategory name parent_id
            return (resultRequest "ok" "Category added", status200))
    else
        return ("Access denied", status401)

-- list
processListCategoriesRequest :: Request -> IO ByteString
processListCategoriesRequest request = do
    -- get params from request
    let frm = fst . fromMaybe (defaultFrom, "") . readInt . fromMaybe "" . join $ lookup "from" $ queryString request :: Int
    let lmt = min defaultLimit . fst . fromMaybe (defaultLimit, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int

    let pp = PaginationParameters lmt frm
    res <- getCategoriesList pp
    total <- totalNumberOfRowsInTable "categories"

    return $ resultCategoriesList res (total, lmt, frm)

-- update
processUpdateCategoryRequest :: Request -> IO (ByteString, Status)
processUpdateCategoryRequest request = do
    -- get params from request
    let query = queryString request

    let c_id = getParam "id" query
    let name = getParam "name" query
    let pid = getParam "parent_id" query
    let token = getParam "token" query

    -- checks
    credentials_correct <- case token of
        Nothing -> return $ Left "Access denied"
        Just x -> checkAdminRightsNew x

    check_id_int <- case c_id of
            Nothing -> return $ Left "id must be set"
            Just x -> case readInt x of
                Nothing -> return $ Left "id incorrect"
                _ -> categoryIdExists $ bytestringToInt . fromJust $ c_id

    let check_pid_int_or_null_or_empty = case pid of
            Nothing -> Right ()
            Just "null" -> Right ()
            Just x -> case readInt x of
                Nothing -> Left "pid incorrect"
                _ -> Right ()


    let check = credentials_correct >> checkAllParametersNothing [name, pid] >> check_id_int >> check_pid_int_or_null_or_empty

    let vals = [("name", fromJust name) | isJust name]
            ++ [("parent_id", fromJust pid) | isJust pid]

    case check of
        Left "Access denied" -> return ("Access denied", status401)
        Left x -> return (resultRequest "error" x, status400)
        _ -> do
            res <- updateCategory (bytestringToInt . fromJust $ c_id) vals
            return (resultRequest "ok" (toString res), status200)

-- POSTS

processAddPostRequest :: Request -> IO (ByteString, Status)
processAddPostRequest request = do
    -- get params from request
    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    --let login = fromMaybe "" . join $ lookup "login" $ queryString request
    let athr = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "author" $ queryString request :: Int
    let shortName = fromMaybe "" . join $ lookup "shortname" $ queryString request
    let category = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "category" $ queryString request :: Int
    let text = fromMaybe "" . join $ lookup "text" $ queryString request
    let mainPicture = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "main_picture" $ queryString request :: Int

    can_create_posts <- canCreatePosts token

    case can_create_posts of
        Left _ -> return ("Access denied", status401)
        _ -> do 
                addPost shortName athr category text mainPicture
                return ("Post added", status200)

processFromDraftRequest :: Request -> IO (ByteString, Status)
processFromDraftRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int

    admin_rights <- checkAdminRights token
    token_accords <- checkPostAndTokenAccordance postID token

    let credentials_correct = admin_rights || token_accords

    if credentials_correct then
        (do _ <- fromDraft postID
            return ("Post published", status200))
    else
        return ("Access denied", status401)

processToDraftRequest :: Request -> IO (ByteString, Status)
processToDraftRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int

    admin_rights <- checkAdminRights token
    token_accords <- checkPostAndTokenAccordance postID token

    let credentials_correct = admin_rights || token_accords

    if credentials_correct then
        (do _ <- toDraft postID
            return ("Post moved to drafts", status200))
    else
        return ("Access denied", status401)

processAddTagToPostRequest :: Request -> IO (ByteString, Status)
processAddTagToPostRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int
    let tagID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "tag_id" $ queryString request :: Int

    admin_rights <- checkAdminRights token
    token_accords <- checkPostAndTokenAccordance postID token

    let credentials_correct = admin_rights || token_accords

    if credentials_correct then
        (do _ <- addTagToPost postID tagID
            return ("Tag added to post", status200))
    else
        return ("Access denied", status401)

processRemoveTagFromPostRequest :: Request -> IO (ByteString, Status)
processRemoveTagFromPostRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int
    let tagID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "tag_id" $ queryString request :: Int

    admin_rights <- checkAdminRights token
    token_accords <- checkPostAndTokenAccordance postID token

    let credentials_correct = admin_rights || token_accords

    if credentials_correct then
        (do _ <- removeTagFromPost postID tagID
            return ("Tag removed from post", status200))
    else
        return ("Access denied", status401)

processAddPictureToPostRequest :: Request -> IO (ByteString, Status)
processAddPictureToPostRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int
    let pictureID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "picture_id" $ queryString request :: Int

    admin_rights <- checkAdminRights token
    token_accords <- checkPostAndTokenAccordance postID token

    let credentials_correct = admin_rights || token_accords

    if credentials_correct then
        (do _ <- addPictureToPost postID pictureID
            return ("Picture added to post", status200))
    else
        return ("Access denied", status401)

processRemovePictureFromPostRequest :: Request -> IO (ByteString, Status)
processRemovePictureFromPostRequest request = do

    let token = fromMaybe "" . join $ lookup "token" $ queryString request
    let postID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "post_id" $ queryString request :: Int
    let pictureID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "picture_id" $ queryString request :: Int

    admin_rights <- checkAdminRights token
    token_accords <- checkPostAndTokenAccordance postID token

    let credentials_correct = admin_rights || token_accords

    if credentials_correct then
        (do _ <- removePictureFromPost postID pictureID
            return ("Picture removed from post", status200))
    else
        return ("Access denied", status401)

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
    let athr = fromMaybe "" . join $ lookup "author" $ queryString request
    let categoryID = fst . fromMaybe (0, "") . readInt . fromMaybe "" . join $ lookup "category_id" $ queryString request :: Int
    let nameContains = fromMaybe "" . join $ lookup "name_contains" $ queryString request
    let textContains = fromMaybe "" . join $ lookup "text_contains" $ queryString request
    let srch = fromMaybe "" . join $ lookup "search" $ queryString request

    -- sorting
    let sortBy = case fromMaybe "" . join $ lookup "sort_by" $ queryString request of
            "date" -> SortByDate
            "author" -> SortByAuthor
            "category" -> SortByCategory
            "pictures_quantity" -> SortByPicturesQuantity
            _ -> SortById

    -- pagination
    let lmt = min defaultLimit . fst . fromMaybe (defaultLimit, "") . readInt . fromMaybe "" . join $ lookup "limit" $ queryString request :: Int
    let frm = fst . fromMaybe (defaultFrom, "") . readInt . fromMaybe "" . join $ lookup "from" $ queryString request :: Int

    let fp = FilteringParameters createdAt createdUntil createdSince athr categoryID nameContains textContains srch
    let sp = SortingParameters sortBy
    let pp = PaginationParameters lmt frm

    res <- getPostsList fp sp pp token showDraft
    total <- totalNumberOfPosts fp
    return $ resultPostsList res (total, lmt, frm)

processUpdatePostRequest :: Request -> IO (ByteString, Status)
processUpdatePostRequest request = do
    -- get params from request
    let query = queryString request

    let post_id = getParam "id" query
    let token = getParam "token" query
    let shortName = getParam "shortname" query
    let category = getParam "category" query
    let text = getParam "text" query
    let mainPicture = getParam "main_picture" query

    -- checks
    let check_id = case post_id of
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
            Just x -> checkPostAndTokenAccordanceNew (bytestringToInt . fromJust $ post_id) x

    let check = checkAllParametersNothing [shortName, category, text, mainPicture] >> check_id >> check_category >> check_main_picture >> credentials_correct

    let vals = [("short_name", fromJust shortName) | isJust shortName]
            ++ [("category", fromJust category) | isJust category]
            ++ [("text", fromJust text) | isJust text]
            ++ [("main_picture", fromJust mainPicture) | isJust mainPicture]

    case check of
        Left "Access denied" -> return ("Access denied", status401)
        Left x -> return (resultRequest "error" x, status400)
        _ -> do
            res <- updatePost (bytestringToInt . fromJust $ post_id) vals
            return (resultRequest "ok" (toString res), status200)

-- TAGS

processAddTagRequest :: Request -> IO ByteString
processAddTagRequest request = do
    -- get params from request
    let name = fromMaybe "" . join $ lookup "name" $ queryString request

    addTag name
