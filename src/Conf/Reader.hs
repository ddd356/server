{-# LANGUAGE OverloadedStrings #-}

module Conf.Reader
    ( Handle (..) ) where

import Data.Configurator.Types
--import Data.ByteString.Char8 ( ByteString )
import Data.ByteString.UTF8 ( ByteString )

newtype Handle = Handle
    { hRead :: Name -> IO ( Maybe ByteString ) }
