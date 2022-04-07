{-# LANGUAGE OverloadedStrings #-}
module Log.Impl.BotLog where

import qualified Log.Handle as L ( config, Handle (..), withHandle, LogLevel, LogMessage )
import Conf.Impl.Common ( newHandle, logLevel, logFilePath )
import Data.Maybe ( fromMaybe )
import Data.ByteString.UTF8 ( toString )
import Data.Char ( toUpper )

log :: L.LogLevel -> L.LogMessage -> IO ()
log level logMessage = do
    confHandle <- newHandle
    maybe_filePath <- logFilePath confHandle
    let filePath = fromMaybe "" maybe_filePath
    maybe_maxLogLevel <- logLevel confHandle
    let maxLogLevel = read . map toUpper . toString $ fromMaybe "" maybe_maxLogLevel

    let c = L.config (toString filePath) maxLogLevel
    L.withHandle c (\handle -> L.log handle level logMessage )
    return ()
