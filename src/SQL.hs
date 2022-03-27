{-# LANGUAGE OverloadedStrings #-}

module SQL where

import Database.PostgreSQL.Simple ( connect, ConnectInfo(..), query, query_, close, execute_, execute, Binary(..) )
import Database.PostgreSQL.Simple.Time ( parseLocalTime )
import qualified Data.Configurator as Cfg ( autoReload, autoConfig, Worth(..), require )
import Data.String ( fromString )
import Data.ByteString ( ByteString )
import Data.ByteString.UTF8 ( toString )
import Data.ByteString.Lazy ( toStrict )
import Random
import Data.Time.LocalTime ( TimeOfDay(..) )
import qualified Data.Text as T ( replace, Text(..), pack, unpack, append, intercalate )
import qualified Data.Text.IO as T ( readFile )
import Data.Aeson ( Value(..), encode )
import Data.Either ( rights )

type SqlUsersList = [(Int, String, String, String, ByteString, TimeOfDay, Bool)]
type SqlPicturesList = [(Int, ByteString)]
type SqlCategoriesList = [(Int, ByteString)]

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
    -- ++ [migration_v...]

migration_v2 :: IO ()
migration_v2 = do
    putStrLn "Migrating to version 2"
    (host, user, password, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user password database )
    
    -- add column "parent_id"
    execute_ conn "ALTER TABLE IF EXISTS public.categories \
\   ADD COLUMN parent_id integer;"

    -- add constraint "parent_id_foreign"
    execute_ conn "ALTER TABLE IF EXISTS public.categories \
\   ADD CONSTRAINT parent_id_foreign FOREIGN KEY (parent_id) \
\   REFERENCES public.categories (id) MATCH SIMPLE \
\   ON UPDATE NO ACTION \
\   ON DELETE NO ACTION \
\   NOT VALID; \
\   CREATE INDEX IF NOT EXISTS fki_parent_id_foreign \
\   ON public.categories(parent_id);"

    -- raise version
    execute_ conn "UPDATE version SET version = 2"

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
    conn2 <- connect ( ConnectInfo host 5432 user password database )
    execute_ conn2 $ "BEGIN; \
\  \
\ CREATE TABLE IF NOT EXISTS public.users \
\ ( \
\     firstname character varying(100), \
\     lastname character varying(100), \
\     avatar bytea, \
\     login character varying(100), \
\     password character varying(100), \
\     create_date time(0) without time zone, \
\     admin boolean, \
\     id serial NOT NULL, \
\     PRIMARY KEY (id) \
\ ); \
\  \
\ CREATE TABLE IF NOT EXISTS public.authors \
\ ( \
\     id serial NOT NULL, \
\     description text, \
\     user_id integer NOT NULL, \
\     PRIMARY KEY (id) \
\ ); \
\  \
\ CREATE TABLE IF NOT EXISTS public.tags \
\ ( \
\     name character varying(100), \
\     id serial NOT NULL, \
\     PRIMARY KEY (id) \
\ ); \
\  \
\ CREATE TABLE IF NOT EXISTS public.news \
\ ( \
\     short_name character varying(300) NOT NULL, \
\     create_date timestamp(0) without time zone, \
\     author integer NOT NULL, \
\     id serial NOT NULL, \
\     category integer NOT NULL, \
\     text text, \
\     main_picture integer, \
\     draft boolean NOT NULL, \
\     PRIMARY KEY (id) \
\ ); \
\  \
\ CREATE TABLE IF NOT EXISTS public.comments \
\ ( \
\     id integer NOT NULL, \
\     news_post integer NOT NULL, \
\     text text, \
\     PRIMARY KEY (id, news_post) \
\ ); \
\  \
\ CREATE TABLE IF NOT EXISTS public.categories \
\ ( \
\     id serial NOT NULL, \
\     name character varying(300) NOT NULL, \
\     PRIMARY KEY (id) \
\ ); \
\  \
\ CREATE TABLE IF NOT EXISTS public.news_tags \
\ ( \
\     news_id integer NOT NULL, \
\     tag_id integer NOT NULL, \
\     PRIMARY KEY (news_id, tag_id) \
\ ); \
\  \
\ CREATE TABLE IF NOT EXISTS public.pictures \
\ ( \
\     id serial NOT NULL, \
\     picture bytea, \
\     PRIMARY KEY (id) \
\ ); \
\  \
\ CREATE TABLE IF NOT EXISTS public.news_pictures \
\ ( \
\     news_id integer NOT NULL, \
\     pictures_id integer NOT NULL, \
\     PRIMARY KEY (news_id, pictures_id) \
\ ); \
\ CREATE TABLE IF NOT EXISTS public.tokens \
\ ( \
\     user_id integer NOT NULL, \
\     token character(20) NOT NULL, \
\     PRIMARY KEY (user_id) \
\ ); \
\ CREATE TABLE IF NOT EXISTS public.version \
\ ( \
\     version integer \
\ ); \
\  \
\ ALTER TABLE IF EXISTS public.authors \
\     ADD FOREIGN KEY (user_id) \
\     REFERENCES public.users (id) MATCH SIMPLE \
\     ON UPDATE NO ACTION \
\     ON DELETE NO ACTION \
\     NOT VALID; \
\  \
\  \
\ ALTER TABLE IF EXISTS public.news \
\     ADD FOREIGN KEY (author) \
\     REFERENCES public.authors (id) MATCH SIMPLE \
\     ON UPDATE NO ACTION \
\     ON DELETE NO ACTION \
\     NOT VALID; \
\  \
\  \
\ ALTER TABLE IF EXISTS public.news \
\     ADD FOREIGN KEY (category) \
\     REFERENCES public.categories (id) MATCH SIMPLE \
\     ON UPDATE NO ACTION \
\     ON DELETE NO ACTION \
\     NOT VALID; \
\  \
\  \
\ ALTER TABLE IF EXISTS public.news \
\     ADD FOREIGN KEY (main_picture) \
\     REFERENCES public.pictures (id) MATCH SIMPLE \
\     ON UPDATE NO ACTION \
\     ON DELETE NO ACTION \
\     NOT VALID; \
\  \
\  \
\ ALTER TABLE IF EXISTS public.comments \
\     ADD FOREIGN KEY (news_post) \
\     REFERENCES public.news (id) MATCH SIMPLE \
\     ON UPDATE NO ACTION \
\     ON DELETE NO ACTION \
\     NOT VALID; \
\  \
\  \
\ ALTER TABLE IF EXISTS public.news_tags \
\     ADD FOREIGN KEY (news_id) \
\     REFERENCES public.news (id) MATCH SIMPLE \
\     ON UPDATE NO ACTION \
\     ON DELETE NO ACTION \
\     NOT VALID; \
\  \
\  \
\ ALTER TABLE IF EXISTS public.news_tags \
\     ADD FOREIGN KEY (tag_id) \
\     REFERENCES public.tags (id) MATCH SIMPLE \
\     ON UPDATE NO ACTION \
\     ON DELETE NO ACTION \
\     NOT VALID; \
\  \
\  \
\ ALTER TABLE IF EXISTS public.news_pictures \
\     ADD FOREIGN KEY (news_id) \
\     REFERENCES public.news (id) MATCH SIMPLE \
\     ON UPDATE NO ACTION \
\     ON DELETE NO ACTION \
\     NOT VALID; \
\  \
\  \
\ ALTER TABLE IF EXISTS public.news_pictures \
\     ADD FOREIGN KEY (pictures_id) \
\     REFERENCES public.pictures (id) MATCH SIMPLE \
\     ON UPDATE NO ACTION \
\     ON DELETE NO ACTION \
\     NOT VALID; \
\  \
\ ALTER TABLE IF EXISTS public.tokens \
\     ADD FOREIGN KEY (user_id) \
\     REFERENCES public.users (id) MATCH SIMPLE \
\     ON UPDATE NO ACTION \
\     ON DELETE NO ACTION \
\     NOT VALID; \
\ \
\ ALTER TABLE IF EXISTS public.users \
\     ADD CONSTRAINT unique_login UNIQUE (login); \
\ \
\ INSERT INTO public.users (firstname, lastname, avatar, login, password, create_date, admin) VALUES ('', '', '' , 'admin', 'admin', '13:00:00', true); \
\ INSERT INTO public.version (version) VALUES (1); \
\ END;"
    close conn2
    return ()

-- USERS

getUsersList :: Int -> Int -> IO SqlUsersList
-- getUsersList :: IO ByteString
getUsersList from limit = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- query_ conn ( fromString $ "SELECT id, firstname, lastname, login, CASE WHEN avatar IS NULL THEN '' ELSE avatar END, create_date, admin FROM public.users ORDER BY id" ++ (if limit > 0 then " LIMIT " ++ (show limit) else "") ++ ("OFFSET " ++ (show from)) ) :: IO SqlUsersList
    close conn
    --return . fromString . unlines . concat $ result
    return result

addUser :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> Bool -> IO ByteString
addUser firstname lastname avatar login password admin = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- execute conn "INSERT INTO public.users (firstname, lastname, avatar, login, password, create_date, admin) VALUES (?, ?, ?, ?, ?, ?, ?)" (firstname, lastname, Binary avatar, login, password, TimeOfDay 13 00 00 , admin)
    close conn
    return "User added"

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

addPost :: ByteString -> ByteString -> Int -> ByteString -> Int -> ByteString -> Int -> IO ByteString
addPost login shortName author createDate category text mainPicture = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_text <- readFile "src\\SQL\\posts\\addPost.sql"
    let create_date = head . rights $ [parseLocalTime "2020-12-21 12:00:00"]
    result <- execute conn (fromString query_text) (shortName, create_date, author, category, text, mainPicture)
    close conn
    return "Post added to drafts"

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

getCategoriesListOld :: Int -> Int -> IO SqlCategoriesList
getCategoriesListOld from limit = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- query_ conn ( fromString $
        "SELECT id, name FROM public.categories ORDER BY id"
        ++ (if limit > 0 then " LIMIT " ++ (show limit) else "") 
        ++ ("OFFSET " ++ (show from)) ) :: IO SqlCategoriesList
    close conn
    return result

getCategoriesList :: IO ByteString
getCategoriesList = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )

    -- get a maximum nested level of categories
    query_text_max_lvl <- readFile "src\\SQL\\categories\\max_lvl.sql"
    res_max_lvl <- query_ conn (fromString query_text_max_lvl) :: IO [[Int]]
    let max_lvl = if length res_max_lvl == 0 then 0 else head . head $ res_max_lvl :: Int

    h <- T.readFile "src\\SQL\\categories\\head.sql"
    t <- T.readFile "src\\SQL\\categories\\tails.sql"
    e <- T.readFile "src\\SQL\\categories\\end.sql"
    
    -- prepare query_text
    let query_text = T.unpack $ categoriesQueryText h t e max_lvl
    putStrLn query_text
    res <- query_ conn ( fromString query_text ) :: IO [[Value]]
    let json = if length res == 0 then "[]" else toStrict . encode . head . head $ res

    close conn

    return json

-- builds query text from head and tail. 
categoriesQueryText :: T.Text -> T.Text -> T.Text -> Int -> T.Text -- head, tail, end, max_lvl
categoriesQueryText h t e m = T.append hd $ T.append tails e where
    hd = T.replace "?" (T.pack . show $ m) h :: T.Text
    tails = T.intercalate ", " . reverse . zipWith placeLevel [0..(m-1)] $ repeat t :: T.Text
    placeLevel l txt = (T.replace "?2" ( intToText $ l+1) $ T.replace "?1" ( intToText l) txt) :: T.Text
    intToText = T.pack . show

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
    result <- query conn "SELECT id from public.users where login = ? AND password = ? LIMIT 1" (login, password) :: IO [[Int]]
    close conn
    return . not . null $ result

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

    -- query for removing existent token for user and add a new token
    res <- execute conn "BEGIN; \
\ \
\ DELETE FROM public.tokens WHERE user_id IN (SELECT id FROM public.users WHERE login = 'admin'); \
\ \
\ INSERT INTO public.tokens \
\ SELECT id, ? FROM public.users WHERE login = ?; \
\ END ;" ( token , login )

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

-- OTHER

totalNumberOfRowsInTable :: String -> IO Int
totalNumberOfRowsInTable table = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_res <- query_ conn . fromString $ "SELECT count(*) FROM " ++ table ++ ";" :: IO [[Int]]
    let res = head . head $ query_res
    close conn
    return res
