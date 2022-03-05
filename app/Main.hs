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
import Data.ByteString.Lazy ( fromStrict )
import Data.Maybe ( fromMaybe )
import Control.Monad ( join )
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
        putStrLn "Processing auth request"
        response_text <- processAuthRequest request
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
            "Wrong path"

processAuthRequest :: Request -> IO ByteString
processAuthRequest request = do
    let query = queryString request
    let action = fromMaybe "" . join $ lookup "action" query
    let user = fromMaybe "" . join $ lookup "login" query
    let password = fromMaybe "" . join $ lookup "password" query
    maybe_token <- case action of
        "auth" -> generateToken user password
        _ -> return $ Just "Wron action parameter"
    return $ fromMaybe "Wrong credentials" maybe_token

generateToken :: ByteString -> ByteString -> IO (Maybe ByteString)
generateToken login password = do
    credentials_correct <- checkCredentials login password
    return $ Just $ fromString $ "credentialsCorrect = " ++ (show credentials_correct)

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
    (config, threadID) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]
    
    -- check news database existense
    news_db_exists <- check_news_db_existense
    putStrLn $ "news_db_exists = " ++ (show news_db_exists)

    -- run server, if all circumstanses are done
    when news_db_exists ( run 8080 app )

create_conf_cfg = do
    -- write new conf file
    writeFile "conf.cfg" "import \"usr_config.cfg\""

