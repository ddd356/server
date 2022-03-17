{-# LANGUAGE OverloadedStrings #-}
module SQL where

import Database.PostgreSQL.Simple ( connect, ConnectInfo(..), query, query_, close, execute_, execute, Binary(..) )
import qualified Data.Configurator as Cfg ( autoReload, autoConfig, Worth(..), require )
import Data.String ( fromString )
import Data.ByteString ( ByteString )
import Data.ByteString.UTF8 ( toString )
import Random
import Data.Time.LocalTime ( TimeOfDay(..) )

type SqlUsersList = [(Int, String, String, String, ByteString, TimeOfDay, Bool)]
type SqlPicturesList = [(Int, ByteString)]

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
\     login character varying(100) CONSTRAINT unique_login UNIQUE, \
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
\  \
\ INSERT INTO public.users (login, password, admin) VALUES ('admin', 'admin', true); \
\ END;"
    close conn2
    return ()

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

checkCredentials :: ByteString -> ByteString -> IO Bool
checkCredentials login password = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- query conn "SELECT id from public.users where login = ? AND password = ? LIMIT 1" (login, password) :: IO [[Int]]
    close conn
    return . not . null $ result

deleteAuthor :: ByteString -> IO ByteString
deleteAuthor login = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- execute conn "DELETE FROM public.authors WHERE user_id IN (SELECT id FROM public.users WHERE users.login = ?)" [login]
    close conn 
    return "Author deleted"

addPost :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> IO ByteString
addPost login shortName createDate category text mainPicture = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- execute conn "INSERT INTO public.pictures ?" [login] -- temporary wrong line
    close conn
    return "Post added to drafts"
    
addPicture :: ByteString -> IO ByteString
addPicture picture = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- execute conn "INSERT INTO public.pictures (picture) VALUES (?)" [picture]
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

checkLoginAndTokenAccordance :: ByteString -> ByteString -> IO Bool
checkLoginAndTokenAccordance login token = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    result <- query conn "SELECT tokens.user_id FROM public.tokens INNER JOIN public.users ON users.id = tokens.user_id WHERE tokens.token = ? AND users.login = ?" (token, login) :: IO [[Int]]
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

totalNumberOfRowsInTable :: String -> IO Int
totalNumberOfRowsInTable table = do
    (host, user, pass, database) <- confGetPostgresqlConfiguration
    conn <- connect ( ConnectInfo host 5432 user pass database )
    query_res <- query_ conn . fromString $ "SELECT count(*) FROM " ++ table ++ ";" :: IO [[Int]]
    let res = head . head $ query_res
    close conn
    return res
