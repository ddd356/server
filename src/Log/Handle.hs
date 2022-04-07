module Log.Handle
    ( LogLevel (..)
    , Config
    , config
    , Handle (..)
    , withHandle
    , LogMessage ) where

import Control.Exception.Base ( bracket )
import Data.Time.Clock ( getCurrentTime )

type LogMessage = String

data LogLevel = OFF | ERROR | WARNING | INFO | DEBUG 
    deriving (Show, Read, Eq, Ord)

data Config = Config
    { cFilePath :: FilePath
    , cMaxLogLevel :: LogLevel } deriving Show

data Handle = Handle
    { log :: LogLevel -> LogMessage -> IO () }

config :: FilePath -> LogLevel -> Config
config filePath maxLogLevel = Config filePath maxLogLevel

new :: Config -> IO Handle
new cfg = return . Handle $ writeLog (cFilePath cfg) (cMaxLogLevel cfg)

writeLog :: FilePath -> LogLevel -> LogLevel -> LogMessage -> IO ()
writeLog filePath maxLogLevel logLevel logMessage = do 
    currentTime <- getCurrentTime
    let logEntry = (show currentTime) ++ " || " ++ (show logLevel) ++ " || " ++ logMessage ++ "\n"
    if logLevel <= maxLogLevel then appendFile filePath logEntry else return ()

close :: Handle -> IO ()
close _ = do
    return ()

withHandle :: Config -> ( Handle -> IO a ) -> IO a
withHandle cfg f = bracket ( new cfg ) close f
