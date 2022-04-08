module Log.Handle
  ( LogLevel (..),
    Config,
    config,
    Handle (..),
    withHandle,
    LogMessage,
  )
where

import Control.Exception.Base (bracket)
import Control.Monad (when)
import Data.Time.Clock (getCurrentTime)

type LogMessage = String

data LogLevel = OFF | ERROR | WARNING | INFO | DEBUG
  deriving (Show, Read, Eq, Ord)

data Config = Config
  { cFilePath :: FilePath,
    cMaxLogLevel :: LogLevel
  }
  deriving (Show)

newtype Handle = Handle
  {log :: LogLevel -> LogMessage -> IO ()}

config :: FilePath -> LogLevel -> Config
config = Config

new :: Config -> IO Handle
new cfg = return . Handle $ writeLog (cFilePath cfg) (cMaxLogLevel cfg)

writeLog :: FilePath -> LogLevel -> LogLevel -> LogMessage -> IO ()
writeLog filePath maxLogLevel logLevel logMessage = do
  currentTime <- getCurrentTime
  let logEntry = show currentTime ++ " || " ++ show logLevel ++ " || " ++ logMessage ++ "\n"
  when (logLevel <= maxLogLevel) $ appendFile filePath logEntry

close :: Handle -> IO ()
close _ = do
  return ()

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle cfg = bracket (new cfg) close
