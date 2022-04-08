module Conf.Reader (Handle (..)) where

import Data.ByteString.UTF8 (ByteString)
import Data.Configurator.Types

newtype Handle = Handle
  {hRead :: Name -> IO (Maybe ByteString)}
