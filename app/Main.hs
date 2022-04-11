{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join, unless, when)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromMaybe)
import Log.Handle
import Log.Impl.BotLog as Log
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import RequestProcessors
import SQL
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import App


main :: IO ()
main = do
  Log.log INFO "Starting a server"

  -- get arguments
  args <- getArgs

  -- create database, if argument -create-db exists
  when ("-create-db" `elem` args) createNewsDb

  -- update database, if argument -update-db exists
  when ("-update-db" `elem` args) updateNewsDb

  -- check conf.cfg existense; create if it is not
  exist_conf_cfg <- doesFileExist "conf.cfg"
  unless exist_conf_cfg createConfCfg

  --  load configuration
  --(config, threadID) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]

  -- check news database existense
  news_db_exists <- checkNewsDBExistense

  -- run server, if all circumstanses are done
  when news_db_exists (run 8080 app)

createConfCfg :: IO ()
createConfCfg = do
  Log.log INFO "Creating new file conf.cfg"

  -- write new conf file
  writeFile "conf.cfg" "import \"usr_config.cfg\""
