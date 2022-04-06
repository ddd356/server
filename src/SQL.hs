{-# LANGUAGE OverloadedStrings #-}

module SQL where

import Database.PostgreSQL.Simple ( connect, ConnectInfo(..), query, query_, close, execute_, execute, Binary(..), Only(..) )
import Database.PostgreSQL.Simple.Time ( parseLocalTime )
import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.ToField ( ToField(..), toField, Action(..) )
import qualified Database.PostgreSQL.Simple.Types as PG ( Null(..) )
import qualified Data.Configurator as Cfg ( autoReload, autoConfig, Worth(..), require )
import Data.String ( fromString )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS ( null, append )
import Data.ByteString.UTF8 ( toString )
import Data.ByteString.Char8 ( readInt )
import Data.ByteString.Lazy ( toStrict )
import Data.ByteString.Builder ( byteString )
import Random
import Data.Time.LocalTime ( TimeOfDay(..), LocalTime(..), getZonedTime, ZonedTime(..) )
import qualified Data.Text as T ( replace, Text(..), pack, unpack, append, intercalate )
import qualified Data.Text.IO as T ( readFile )
import Data.Aeson ( Value(..), encode )
import Data.Either ( rights )
import Data.Maybe ( isNothing, fromJust )
import Data.List ( foldl', intersperse, intercalate )
import Crypto.PBKDF ( sha512PBKDF2 )

type SqlUsersList = [(Int, String, String, String, ByteString, TimeOfDay, Bool, Bool)]
type SqlPicturesList = [(Int, ByteString)]
type SqlCategoriesList = [(Int, ByteString)]

type Token = ByteString

data FilteringParameters = FilteringParameters {
    created_at :: Maybe LocalTime,
    created_until :: Maybe LocalTime,
    created_since :: Maybe LocalTime,
    author :: ByteString,
    category_id :: Int,
    name_contains :: ByteString,
    text_contains :: ByteString,
    search :: ByteString
}

data PaginationParameters = PaginationParameters {
    limit :: Int,
    from :: Int
}

data SortingParameters = SortingParameters {
    sort_by :: SortingField
} deriving Show

data SortingField = SortByDate | SortByAuthor | SortByCategory | SortByPicturesQuantity | SortById deriving (Eq, Show)

instance ToRow FilteringParameters where
    toRow fp = []
        ++ (if isNothing p1 then [] else [toField p1])
        ++ (if isNothing p2 then [] else [toField p2])
        ++ (if isNothing p3 then [] else [toField p3])
        ++ (if BS.null p4 then [] else [toField p4] ++ [toField p4]) -- two same wildcards
        ++ (if p5 == 0 then [] else [toField p5])
        ++ (if BS.null p6 then [] else [toField $ ilike p6])
        ++ (if BS.null p7 then [] else [toField $ ilike p7])
        ++ (if BS.null p8 then [] else [toField $ ilike p8] ++ [toField $ ilike p8] ++ [toField $ ilike p8] ++ [toField $ ilike p8]) where
            p1 = created_at fp
            p2 = created_until fp
            p3 = created_since fp
            p4 = author fp
            p5 = category_id fp
            p6 = name_contains fp
            p7 = text_contains fp
            p8 = search fp
            ilike x = "%" `BS.append` x `BS.append` "%"

instance ToRow PaginationParameters where
    toRow pp = toField p1 : toField p2 : [] where
            p1 = if limit pp > 50 || limit pp < 1 then 50 else limit pp
            p2 = from pp

instance ToRow SortingParameters where
    toRow sp = []
        ++ [toField p1] where
            p1 = sort_by sp

instance ToField SortingField where
    toField SortByDate = Plain ( byteString "create_date")
    toField SortByAuthor = Plain ( byteString "author" )
    toField SortByCategory = Plain ( byteString "category" )
    toField SortByPicturesQuantity = Plain ( byteString "(SELECT count(*) FROM news_pictures WHERE news_id = news.id)" )
    toField SortById = Plain ( byteString "id" ) -- default

check_news_db_existense :: IO Bool
check_news_db_existense = do
    (host, user, password, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user password "" )
    db_names <- query_ conn "SELECT datname FROM pg_database;" :: IO [[String]]
    database <- confGetPostgresqlDatabase
    close conn
    case elem [database] db_names of
        True -> return True
        False -> do
            putStrLn "Database doesn't exist on a server. Run with -create-db to initialize database."
            return False

confGetPostgresqlConfiguration :: IO (String, String, String, String)
confGetPostgresqlConfiguration = do
    host <- confGetPostgresqlHost
    user <- confGetPostgresqlUser
    password <- confGetPostgresqlPassword
    database <- confGetPostgresqlDatabase
    return (host, user, password, database)

confGetPostgresqlHost :: IO String
confGetPostgresqlHost = do
    (config, threadID) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]
    Cfg.require config "host"

confGetPostgresqlUser :: IO String
confGetPostgresqlUser = do
    (config, threadID) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]
    Cfg.require config "user"

confGetPostgresqlPassword :: IO String
confGetPostgresqlPassword = do
    (config, threadID) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]
    Cfg.require config "password"

confGetPostgresqlDatabase :: IO String
confGetPostgresqlDatabase = do
    (config, threadID) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]
    Cfg.require config "database"

-- UPDATING DB

updateNewsDb :: IO ()
updateNewsDb = do
    res <- getVersion
    let version = head . head $ res
    sequence_ $ drop (version-1) migrations

migrations :: [ IO () ]
migrations = []
    ++ [migration_v2]
    ++ [migration_v3]
    ++ [migration_v4]
    -- ++ [migration_v...]

migration_v2 :: IO ()
migration_v2 = do
    putStrLn "Migrating to version 2"
    (host, user, password, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user password database )
    
    -- add column "parent_id"
    query_text_1 <- readFile "src\\SQL\\migrations\\v2\\add_column_parent_id.sql"
    execute_ conn (fromString query_text_1)

    -- add constraint "parent_id_foreign"
    query_text_2 <- readFile "src\\SQL\\migrations\\v2\\add_constraint_parent_id_foreign.sql"
    execute_ conn (fromString query_text_2)

    -- raise version
    execute_ conn "UPDATE version SET version = 2"

    close conn
    return ()

migration_v3 :: IO ()
migration_v3 = do
    putStrLn "Migration to version 3"
    (host, user, password, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user password database )

    -- add function jsonb_cat_with_parents
    query_text_1 <- readFile "src\\SQL\\migrations\\v3\\add_function_jsonb_cat_with_parents.sql"
    execute_ conn (fromString query_text_1)

    -- add function jsonb_post_pictures
    query_text_2 <- readFile "src\\SQL\\migrations\\v3\\add_function_jsonb_post_pictures.sql"
    execute_ conn (fromString query_text_2)

    -- add function jsonb_post_tags
    query_text_3 <- readFile "src\\SQL\\migrations\\v3\\add_function_jsonb_post_tags.sql"
    execute_ conn (fromString query_text_3)

    -- add function uri_picture
    query_text_4 <- readFile "src\\SQL\\migrations\\v3\\add_function_uri_picture.sql"
    execute_ conn (fromString query_text_4)

    -- raise version
    execute_ conn "UPDATE version SET version = 3"
    close conn
    return ()

migration_v4 :: IO ()
migration_v4 = do
    putStrLn "Migration to version 4"
    (host, user, password, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user password database )

    -- add new column "can_create_posts" to users table
    query_text <- readFile "src\\SQL\\migrations\\v4\\add_field_can_create_posts.sql"
    execute_ conn (fromString query_text)

    --raise version
    execute_ conn "UPDATE version SET version = 4"
    close conn
    return ()
    
getVersion :: IO [[Int]]
getVersion = do
    (host, user, password, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user password database )
    res <- query_ conn "SELECT version FROM public.version;" :: IO [[Int]]
    close conn
    return res

-- CREATING DB

createNewsDb :: IO ()
createNewsDb = do
    (host, user, password, database) <- confGetPostgresqlConfiguration
    conn1 <- connect ( ConnectInfo host 5432 user password "" )
    execute_ conn1 $ fromString $ "CREATE DATABASE " ++ database ++ ";"
    close conn1

    query_text <- readFile "src\\SQL\\migrations\\init\\init.sql"
    conn2 <- connect ( ConnectInfo host 5432 user password database )

    salt <- getRandomString 10
    let hash = salt ++ (sha512PBKDF2 "admin" salt 2 45)

    zoned_time <- getZonedTime
    let create_date = zonedTimeToLocalTime zoned_time

    execute conn2 (fromString query_text) (hash, create_date)
    close conn2
    return ()

-- USERS

getUsersList :: Int -> Int -> IO SqlUsersList
getUsersList from limit = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- query_ conn ( fromString $ "SELECT id, firstname, lastname, login, CASE WHEN avatar IS NULL THEN '' ELSE avatar END, create_date, admin, can_create_posts FROM public.users ORDER BY id" ++ (if limit > 0 then " LIMIT " ++ (show limit) else "") ++ ("OFFSET " ++ (show from)) ) :: IO SqlUsersList
    close conn
    return result

addUser :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> Bool -> Bool -> IO ByteString
addUser firstname lastname avatar login password admin can_create_posts = do

    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )

    salt <- getRandomString 10
    let hash = salt ++ (sha512PBKDF2 (toString password) salt 2 45)

    zoned_time <- getZonedTime
    let create_date = zonedTimeToLocalTime zoned_time

    query_text <- readFile "src\\SQL\\users\\addUser.sql"
    result <- execute conn (fromString query_text) (firstname, lastname, Binary avatar, login, hash, create_date, admin, can_create_posts)
    close conn
    return "User added"

loginNotExists :: ByteString -> IO (Either String ()) 
loginNotExists login = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    res <- query conn "SELECT 1 FROM users WHERE login = ?" [login] :: IO [[Int]]
    close conn
    return $ case res of
        [] -> Right ()
        _ -> Left "User with that login already exists"

-- AUTHORS

deleteAuthor :: ByteString -> IO ByteString
deleteAuthor login = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- execute conn "DELETE FROM public.authors WHERE user_id IN (SELECT id FROM public.users WHERE users.login = ?)" [login]
    close conn 
    return "Author deleted"

addAuthor :: ByteString -> ByteString -> IO ByteString
addAuthor login description = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- execute conn "INSERT INTO public.authors (description, user_id) \
\ SELECT ?, id \
\ FROM public.users \
\ WHERE users.login = ?" (description, login)
    close conn
    return "Author added"

-- POSTS

addPost :: ByteString -> Int -> ByteString -> Int -> ByteString -> Int -> IO ()
addPost shortName author createDate category text mainPicture = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_text <- readFile "src\\SQL\\posts\\addPost.sql"
    let create_date = head . rights $ [parseLocalTime "2020-12-21 12:00:00"]
    result <- execute conn (fromString query_text) (shortName, create_date, author, category, text, mainPicture)
    close conn
    return ()

fromDraft :: Int -> IO ByteString
fromDraft postID = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_text <- readFile "src\\SQL\\posts\\fromDraft.sql"
    result <- execute conn (fromString query_text) [postID]
    putStrLn $ show result
    close conn
    return "Post published from drafts"

toDraft :: Int -> IO ByteString
toDraft postID = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_text <- readFile "src\\SQL\\posts\\toDraft.sql"
    result <- execute conn (fromString query_text) [postID]
    putStrLn $ show result
    close conn
    return "Post moved to drafts"

addTagToPost :: Int -> Int -> IO ByteString
addTagToPost postID tagID = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_text <- readFile "src\\SQL\\posts\\addTagToPost.sql"
    res <- execute conn (fromString query_text) (postID, tagID)
    close conn
    return "Tag added to post"
 
removeTagFromPost :: Int -> Int -> IO ByteString
removeTagFromPost postID tagID = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_text <- readFile "src\\SQL\\posts\\removeTagFromPost.sql"
    res <- execute conn (fromString query_text) (postID, tagID)
    close conn
    return "Tag removed from post"

addPictureToPost :: Int -> Int -> IO ByteString
addPictureToPost postID pictureID = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_text <- readFile "src\\SQL\\posts\\addPictureToPost.sql"
    res <- execute conn (fromString query_text) (postID, pictureID)
    close conn
    return "Picture added to post"

removePictureFromPost :: Int -> Int -> IO ByteString
removePictureFromPost postID pictureID = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_text <- readFile "src\\SQL\\posts\\removePictureFromPost.sql"
    res <- execute conn (fromString query_text) (postID, pictureID)
    close conn
    return "Picture removed from post"

getPostsList :: FilteringParameters -> SortingParameters -> PaginationParameters -> Token -> Bool -> IO ByteString
getPostsList fp sp pp token showDraft = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    template_1 <- readFile "src\\SQL\\posts\\getPostsList_1.sql"
    template_2 <- readFile "src\\SQL\\posts\\getPostsList_2.sql"
    
    let query_text =
            template_1
            -- sorting parameters
            -- ++ (if sort_by sp == SortByNothing then "" else " ORDER BY ?")
            -- pagination parameters
            -- ++ (if from pp == 0 then "" else " OFFSET ?")
            -- ++ (if limit pp == 0 then "" else " FETCH ?")
            -- filtering parameters
            ++ (if isNothing $ created_at fp then "" else " AND create_date = ?")
            ++ (if isNothing $ created_until fp then "" else " AND create_date <= ?")
            ++ (if isNothing $ created_since fp then "" else " AND create_date >= ?")
            ++ (if BS.null $ author fp then "" else " AND (subquery1.firstname = ? OR subquery1.lastname = ?)")
            ++ (if category_id fp == 0 then "" else " AND category = ?")
            ++ (if BS.null $ name_contains fp then "" else " AND short_name ILIKE ?")
            ++ (if BS.null $ text_contains fp then "" else " AND text ILIKE ?")
            ++ (if BS.null $ search fp then "" else " AND (subquery1.firstname ILIKE ? OR subquery1.lastname = ? OR short_name ILIKE ? OR text ILIKE ?)")
            ++ template_2
 
    putStrLn query_text
    putStrLn . show $ sp
    putStrLn . show $ [toField token] ++ toRow fp ++ toRow sp ++ toRow pp
    res <- query conn (fromString query_text) ([toField token] ++ [toField showDraft] ++ toRow fp ++ toRow sp ++ toRow pp) :: IO [[Value]]
    let json = if length res == 0 then "[]" else toStrict . encode . head . head $ res

    close conn
    return json 

updatePost :: Int -> [(String, ByteString)] -> IO ByteString
updatePost id vals = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )

    let tmplt =  (++ " = ?")
    let (pnames, pvals) = foldl' f ([],[]) vals where
        f (ns, vs) (n, v) = case n of
             "category" -> (tmplt n : ns, (if v == "null" then toField PG.Null else toField . bytestringToInt $ v) : vs)
             "main_picture" -> (tmplt n : ns, (if v == "null" then toField PG.Null else toField . bytestringToInt $ v) : vs)
             _ -> (tmplt n : ns, (toField v) : vs)

    let set_text = intercalate ", " pnames
    let pvals' = pvals ++ [toField id]

    let query_text = "UPDATE news SET "
            ++ set_text
            ++ " WHERE id = ?"
    execute conn (fromString query_text) (pvals')
    close conn
    return "Post updated"

testQ :: IO ()
testQ = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    res <- query conn (fromString "SELECT ? + ?") ( [2,2] :: [Int]) :: IO [[Int]]
    putStrLn . show $ res

-- PICTURES
    
addPicture :: ByteString -> IO ByteString
addPicture picture = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- execute conn "INSERT INTO public.pictures (picture) VALUES (?)" [Binary picture]
    close conn
    return "Picture added"

getPicturesList :: Int -> Int -> IO SqlPicturesList
getPicturesList from limit = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- query_ conn ( fromString $ 
        "SELECT id, picture FROM public.pictures ORDER BY id"
        ++ (if limit > 0 then " LIMIT " ++ (show limit) else "") 
        ++ ("OFFSET " ++ (show from)) ) :: IO SqlPicturesList
    close conn
    return result

getPicture :: Int -> IO ByteString
getPicture id = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_text <- readFile "src\\SQL\\pictures\\getPicture.sql"
    result <- query conn ( fromString query_text ) [id] :: IO [[ByteString]]
    close conn
    case null result of
        True -> return ""
        _ -> return $ head . head $ result

pictureIdExists :: Int -> IO (Either String ())
pictureIdExists id = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    res <- query conn "SELECT id FROM pictures WHERE id = ?" [id] :: IO [[Int]]
    close conn
    return $ case res of
        [] -> Left "Picture with that ID don't exist"
        _ -> Right ()

-- CATEGORIES

addCategory :: ByteString -> Int -> IO ByteString
addCategory name parent_id = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- case parent_id of
        0 -> execute conn "INSERT INTO public.categories (name) VALUES (?)" [name]
        _ -> execute conn "INSERT INTO public.categories (name, parent_id) VALUES (?, ?)" (name, parent_id)
    close conn
    return "Category added"

getCategoriesList :: PaginationParameters -> IO ByteString
getCategoriesList pp = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_text <- readFile "src\\SQL\\categories\\getCategoriesList.sql"

    res <- query conn ( fromString query_text) (toRow pp) :: IO [[Value]]
    let json = if length res == 0 then "[]" else toStrict . encode . head . head $ res

    close conn
    return json

--getCategoriesListOld :: IO ByteString
--getCategoriesListOld = do
--    (host, user, pass, database) <- confGetPostgresqlConfiguration
--    conn <- connect ( ConnectInfo host 5432 user pass database )
--
--    -- get a maximum nested level of categories
--    query_text_max_lvl <- readFile "src\\SQL\\categories\\max_lvl.sql"
--    res_max_lvl <- query_ conn (fromString query_text_max_lvl) :: IO [[Int]]
--    let max_lvl = if length res_max_lvl == 0 then 0 else head . head $ res_max_lvl :: Int
--
--    h <- T.readFile "src\\SQL\\categories\\head.sql"
--    t <- T.readFile "src\\SQL\\categories\\tails.sql"
--    e <- T.readFile "src\\SQL\\categories\\end.sql"
--    
--    -- prepare query_text
--    let query_text = T.unpack $ categoriesQueryText h t e max_lvl
--    putStrLn query_text
--    res <- query_ conn ( fromString query_text ) :: IO [[Value]]
--    let json = if length res == 0 then "[]" else toStrict . encode . head . head $ res
--
--    close conn
--
--    return json
--
---- builds query text from head and tail. 
--categoriesQueryText :: T.Text -> T.Text -> T.Text -> Int -> T.Text -- head, tail, end, max_lvl
--categoriesQueryText h t e m = T.append hd $ T.append tails e where
--    hd = T.replace "?" (T.pack . show $ m) h :: T.Text
--    tails = T.intercalate ", " . reverse . zipWith placeLevel [0..(m-1)] $ repeat t :: T.Text
--    placeLevel l txt = (T.replace "?2" ( intToText $ l+1) $ T.replace "?1" ( intToText l) txt) :: T.Text
--    intToText = T.pack . show

bytestringToInt :: ByteString -> Int
bytestringToInt = fst . fromJust . readInt

updateCategory :: Int -> [(String, ByteString)] -> IO ByteString
updateCategory id vals = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    
    let tmplt =  (++ " = ?")
    let (pnames, pvals) = foldl' f ([],[]) vals where
        f (ns, vs) (n, v) = case n of
             "pid" -> (tmplt n : ns, (if v == "null" then toField PG.Null else toField . bytestringToInt $ v) : vs)
             _ -> (tmplt n : ns, (toField v) : vs)

    let set_text = intercalate ", " pnames
    let pvals' = pvals ++ [toField id]

    let query_text = "UPDATE categories SET "
            ++ set_text
            ++ " WHERE id = ?"
    execute conn (fromString query_text) (pvals')
    close conn
    return "Category updated"

categoryIdExists :: Int -> IO (Either String ())
categoryIdExists id = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    res <- query conn "SELECT id FROM categories WHERE id = ?" [id] :: IO [[Int]]
    close conn
    return $ case res of
        [] -> Left "Category with that id don't exist"
        _ -> Right ()


-- TAGS

addTag :: ByteString -> IO ByteString
addTag name = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    res <- execute conn "INSERT INTO public.tags (name) VALUES (?)" [name]
    close conn
    return "Tag added"

-- CHECK CREDENTIALS

checkCredentials :: ByteString -> ByteString -> IO Bool
checkCredentials login password = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    --let hash = salt ++ (sha512PBKDF2 (toString password) salt 2 45)
    result <- query conn "SELECT password FROM users WHERE login = ? LIMIT 1" [login] :: IO [[String]]
    close conn
    return $ case result of
        [[ hash ]] -> (sha512PBKDF2 (toString password) (take 10 hash) 2 45) == drop 10 hash
        _ -> False

checkLoginAndTokenAccordance :: ByteString -> ByteString -> IO Bool
checkLoginAndTokenAccordance login token = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- query conn "SELECT tokens.user_id FROM public.tokens INNER JOIN public.users ON users.id = tokens.user_id WHERE tokens.token = ? AND users.login = ?" (token, login) :: IO [[Int]]
    close conn
    return . not . null $ result

checkPostAndTokenAccordance :: Int -> ByteString -> IO Bool
checkPostAndTokenAccordance postID token = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_text <- readFile "src\\SQL\\postAndTokenAccordance.sql"
    result <- query conn (fromString query_text) [postID] :: IO [[Int]]
    close conn
    return . not . null $ result
    
checkPostAndTokenAccordanceNew :: Int -> ByteString -> IO (Either String ())
checkPostAndTokenAccordanceNew postID token = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_text <- readFile "src\\SQL\\postAndTokenAccordance.sql"
    result <- query conn (fromString query_text) [postID] :: IO [[Int]]
    close conn
    return $ if null result then Left("You have no access") else Right()  

generateToken :: ByteString -> IO ByteString
generateToken login = do
    -- read parameters from config
    (host, user, pass, database) <- confGetPostgresqlConfiguration

    -- generating random token
    newToken <- getRandomString 20

    -- open connection
    conn <- connect ( ConnectInfo host 5432 user pass database )

    -- query for checking token existense
    res <- query conn "SELECT token FROM public.tokens WHERE token = ?" [newToken] :: IO [[String]]
    if length res > 0
        then generateToken login
        else return . fromString $ newToken

addNewToken :: ByteString -> ByteString -> IO ()
addNewToken login token = do
    -- read parameters from config
    (host, user, pass, database) <- confGetPostgresqlConfiguration

    -- open connection
    conn <- connect ( ConnectInfo host 5432 user pass database )

    query_text <- readFile "src\\SQL\\auth\\addNewToken.sql"
    res <- execute conn (fromString query_text) ( login, token, login )

    -- closing connection
    close conn

    return ()

checkAdminRights :: ByteString -> IO Bool
checkAdminRights token = do
    -- read parameters from config
    (host, user, pass, database) <- confGetPostgresqlConfiguration

    -- open connection
    conn <- connect ( ConnectInfo host 5432 user pass database )

    -- query for checking admin rights
    res <- query conn "SELECT tokens.user_id FROM tokens INNER JOIN users ON tokens.user_id = users.id WHERE tokens.token = ? AND users.admin" [token] :: IO [[Int]]
    close conn
    return $ length res /= 0

checkAdminRightsNew :: ByteString -> IO (Either String ())
checkAdminRightsNew token = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )

    res <- query conn "SELECT 1 FROM tokens INNER JOIN users ON tokens.user_id = users.id WHERE tokens.token = ? AND users.admin" [token] :: IO [[Int]]
    close conn
    return $ if (length res /= 0) then Right () else Left "Access denied"

canCreatePosts :: Token -> IO (Either String ())
canCreatePosts token = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )

    res <- query conn "SELECT 1 FROM tokens INNER JOIN users ON tokens.user_id = users.id WHERE tokens.token = ? AND users.can_create_posts" [token] :: IO [[Int]]

    close conn
    return $ if (length res /= 0) then Right () else Left "Access denied"

-- OTHER

totalNumberOfRowsInTable :: String -> IO Int
totalNumberOfRowsInTable table = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_res <- query_ conn . fromString $ "SELECT count(*) FROM " ++ table ++ ";" :: IO [[Int]]
    let res = head . head $ query_res
    close conn
    return res

totalNumberOfPosts :: FilteringParameters -> IO Int
totalNumberOfPosts fp = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    template <- readFile "src\\SQL\\posts\\totalNumberOfPosts.sql"
    let query_text = template
            ++ (if isNothing $ created_at fp then "" else " AND create_date = ?")
            ++ (if isNothing $ created_until fp then "" else " AND create_date <= ?")
            ++ (if isNothing $ created_since fp then "" else " AND create_date >= ?")
            ++ (if BS.null $ author fp then "" else " AND (subquery1.firstname ILIKE ? OR subquery1.lastname ILIKE ?)")
            ++ (if category_id fp == 0 then "" else " AND category = ?")
            ++ (if BS.null $ name_contains fp then "" else " AND short_name ILIKE ?")
            ++ (if BS.null $ text_contains fp then "" else " AND text ILIKE ?") 
            ++ (if BS.null $ search fp then "" else " AND (subquery1.firstname ILIKE ? OR subquery1.lastname = ? OR short_name ILIKE ? OR text ILIKE ?)")
    query_res <- query conn (fromString query_text) (toRow fp) :: IO [[Int]]
    let res = head . head $ query_res
    close conn
    return res
