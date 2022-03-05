module SQL.Handle where

import Database.PostgreSQL.Simple ( Query, query_, execute_, connect, ConnectInfo(..) )
--import qualified Database.PostgreSQL.Simple (close) as PG
import Data.Int ( Int64 )
import Data.Word ( Word16 )

data Handle = Handle
    { hQuery :: Query -> IO [[String]]
    , hExecute :: Query -> IO Int64 }

data Config = Config
    { host :: String
    , port :: Word16
    , user :: String
    , password :: String
    , database :: String }

config :: String -> Word16 -> String -> String -> String -> Config
config host port user password database = Config host port user password database

new :: Config -> IO Handle
new config = do
    conn <- connect ( ConnectInfo (host config) (port config) (user config) (password config) (database config) )
    return $ Handle ( query_ conn ) ( execute_ conn )

--close :: Handle -> IO ()
--close h = do
--    PG.close h
