{-# LANGUAGE OverloadedStrings #-}

module Log.Impl.BotLog where

import Conf.Impl.Common (logFilePath, logLevel, newHandle)
import Data.ByteString.UTF8 (toString)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import qualified Log.Handle as L (Handle (..), LogLevel, LogMessage, config, withHandle)

log :: L.LogLevel -> L.LogMessage -> IO ()
log level logMessage = do
  confHandle <- newHandle
  maybe_filePath <- logFilePath confHandle
  let filePath = fromMaybe "" maybe_filePath
  maybe_maxLogLevel <- logLevel confHandle
  let maxLogLevel = read . map toUpper . toString $ fromMaybe "" maybe_maxLogLevel

  let c = L.config (toString filePath) maxLogLevel
  L.withHandle c (\handle -> L.log handle level logMessage)
  return ()
