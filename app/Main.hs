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
import Database.PostgreSQL.Simple ( connect, ConnectInfo(..), query_, close, execute_)
import System.Environment ( getArgs )
import Data.String ( fromString )

app :: Application
app request respond
    | length (pathInfo request) == 1 && last (pathInfo request) == "posts" = do
        -- return posts
        putStrLn "I've done some IO here"
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            "Here must be returned posts"
    | length (pathInfo request) == 1 && last (pathInfo request) == "auth" = do
        -- print message about wrong guard
        putStrLn "I've done some IO here"
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            "Here must be an auth endpoint"
        
    | otherwise = do
        -- print info message to command line
        putStrLn "I've done some IO here"
        respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            "Wrong path"

main :: IO ()
main = do
    -- get arguments
    args <- getArgs

    -- create database, if argument -create-db exists
    when ( elem "-create-db" args ) createNewsDb

    --  check conf.cfg existense; create if it is not
    exist_conf_cfg <- doesFileExist "conf.cfg"
    when (not exist_conf_cfg) create_conf_cfg
    
    --  load configuration

    --      loading conf.cfg
    (config, threadID) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]

    
    -- check news database existense
    news_db_exists <- check_news_db_existense
    putStrLn $ "news_db_exists = " ++ (show news_db_exists)

    -- run server, if all circumstanses are done
    when news_db_exists ( run 8080 app )

create_conf_cfg = do
    -- write new conf file
    writeFile "conf.cfg" "import \"usr_config.cfg\""

check_news_db_existense :: IO Bool
check_news_db_existense = do
    host <- confGetPostgresqlHost
    user <- confGetPostgresqlUser
    password <- confGetPostgresqlPassword
    conn <- connect ( ConnectInfo host 5432 user password "" )
    db_names <- query_ conn "SELECT datname FROM pg_database;" :: IO [[String]]
    database <- confGetPostgresqlDatabase
    close conn
    case elem [database] db_names of
        True -> return True
        False -> do
            putStrLn "Database doesn't exist on a server. Run with -create-db to initialize database."
            return False

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
    host <- confGetPostgresqlHost
    user <- confGetPostgresqlUser
    password <- confGetPostgresqlPassword
    database <- confGetPostgresqlDatabase
    conn1 <- connect ( ConnectInfo host 5432 user password "" )
    execute_ conn1 $ fromString $ "CREATE DATABASE " ++ database ++ ";"
    close conn1
    conn2 <- connect ( ConnectInfo host 5432 user password database )
    execute_ conn2 $ "BEGIN; \
\  \
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
\ END;"
    close conn2
    return ()
