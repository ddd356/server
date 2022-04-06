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
config h p u pwd db = Config h p u pwd db

new :: Config -> IO Handle
new c = do
    conn <- connect ( ConnectInfo (host c) (port c) (user c) (password c) (database c) )
    return $ Handle ( query_ conn ) ( execute_ conn )

--close :: Handle -> IO ()
--close h = do
--    PG.close h
