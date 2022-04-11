import Control.Exception ( try )
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Monadic as QM
import Network.HTTP.Simple
import qualified App as A (app)
import qualified SQL as S (createNewsDb)
import qualified Network.Wai.Handler.Warp as W (run)
import Control.Concurrent.Async ( withAsync, cancel )
import Database.PostgreSQL.Simple (Binary (..), ConnectInfo (..), close, connect, execute, execute_, query, query_)
import SQL
import Data.String (fromString)
import System.FilePath ((</>))

test_database :: String
test_database = "news_test_database"

prop_CreateDatabase = QM.monadicIO $ do
        res <- QM.run           -- m a -> PropertyM m a
            (try                -- IO (Either e a)
            (createTestNewsDb)) -- IO ()
        QM.assert $ case res of
            Left _ -> False
            _ -> True

main :: IO ()
main = do
    withAsync (W.run 8080 A.app) $ \a1 -> do
        --request <- parseRequest "http://localhost:8080/users?action=list"
        --response <- httpBS request
        --print response
        Q.quickCheck prop_CreateDatabase
        cancel a1
        return ()

createTestNewsDb :: IO ()
createTestNewsDb = do

  (host, user, password, _) <- confGetPostgresqlConfiguration
  conn1 <- connect (ConnectInfo host 5432 user password "")
  _ <- execute_ conn1 $ fromString $ "CREATE DATABASE " ++ test_database ++ ";"
  close conn1

  query_text <- readFile $ "src" </> "SQL" </> "migrations" </> "init" </> "init.sql"
  conn2 <- connect (ConnectInfo host 5432 user password test_database)

  _ <- execute_ conn2 (fromString query_text)
  close conn2

  updateNewsDb
  addAdmin

  return ()
