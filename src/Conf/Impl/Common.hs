{-# LANGUAGE OverloadedStrings #-}

module Conf.Impl.Common (
    newHandle,
    useBot,
    logLevel,
    logFilePath ) where

import qualified Conf.Reader as R
import Data.Configurator as C
import Data.Configurator.Types
import qualified Data.Text as T ( append )
import qualified Data.Text.IO as T ( putStrLn )
import Data.ByteString.UTF8 ( ByteString )

newHandle :: IO R.Handle
newHandle = do
    return $ R.Handle
        { R.hRead = getConfElement }

useBot :: R.Handle -> IO ( Maybe ByteString )
useBot = (`R.hRead` "use_bot")

logLevel :: R.Handle -> IO ( Maybe ByteString )
logLevel = (`R.hRead` "log_level")

logFilePath :: R.Handle -> IO ( Maybe ByteString )
logFilePath = (`R.hRead` "log_filepath")

getConfElement :: Name -> IO (Maybe ByteString)
getConfElement element_name = do
    config <- C.load [Required "conf.cfg"]
    conf_element <- C.lookup config element_name
    case conf_element of
        Nothing -> do
            T.putStrLn $ "ERROR: can't load " `T.append` element_name `T.append` "; check conf.cfg" 
            return Nothing
        x -> return x
