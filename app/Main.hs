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

app :: Application
app _ respond = do
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

    -- run server
    run 8080 app

create_conf_cfg = do
    writeFile "conf.cfg" "import \"usr_config.cfg\""
