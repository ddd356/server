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
import Database.PostgreSQL.Simple ( connect, ConnectInfo(..) )

app :: Application
app request respond = do
    -- print info message to command line
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

main :: IO ()
main = do
    --  check conf.cfg existense; create if it is not
    exist_conf_cfg <- doesFileExist "conf.cfg"
    when (not exist_conf_cfg) create_conf_cfg
    
    --  load configuration

    --      loading conf.cfg
    (config, threadID) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]

    
    -- check news database existense
    let news_db_exists = check_news_db_existense
    putStrLn $ "news_db_exists = " ++ (show news_db_exists)

    -- run server
    run 8080 app

create_conf_cfg = do
    -- write new conf file
    writeFile "conf.cfg" "import \"usr_config.cfg\""

check_news_db_existense :: Bool
check_news_db_existense = do
    --let host = confGetPostgresqlHost
    --let connection = connect ( ConnectInfo host 5432 "" "" "" )
    True

--confGetPostgresqlHost :: String
