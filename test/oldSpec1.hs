{-# LANGUAGE OverloadedStrings #-}

-- TODO look implemented tests for a bot
--import qualified  Control.Monad.Reader ()

import qualified App as A (app)
import qualified Control.Concurrent.Async as Async (cancel, withAsync)
import qualified Control.Exception as E (Exception, SomeException (..))
import qualified Control.Exception.Base as EB (catch, throw)
import qualified Control.Monad.Trans.Reader as TR (ReaderT (..), ask, runReaderT)
import Data.String (fromString)
import Database.PostgreSQL.Simple (ExecStatus (..), FormatError (..), QueryError (..), ResultError (..), SqlError (..))
import qualified Database.PostgreSQL.Simple as PGS (ConnectInfo (..), close, connect, execute_)
import Env
import qualified Network.Wai.Handler.Warp as W (run)
import qualified SQL (addAdmin, confGetPostgresqlConfiguration, updateNewsDb)
import System.FilePath ((</>))
import qualified Test.QuickCheck as Q (Args (..), Property (..), quickCheck, quickCheckWith, stdArgs)
import qualified Test.QuickCheck.Monadic as QM (assert, monadicIO, run)

data MyException = ErrorCreatingDatabase | OtherError
  deriving (Show)

data PGConfig = PGConfig
  { pg_host :: String,
    pg_user :: String,
    pg_password :: String,
    pg_database :: String
  }

instance E.Exception MyException

prop_CreateTestDatabase :: Q.Property
prop_CreateTestDatabase = QM.monadicIO $ do
  -- result <- QM.run $ EB.catch ( return True ) ( \e -> do putStrLn ("Caugh " ++ show (e :: QueryExceptions) ) ; return False  )
  -- TODO implement EB.catches for all query exceptions
  result <- QM.run $ EB.catch (EB.throw $ ErrorCreatingDatabase) (\e -> do putStrLn ("Caugh " ++ show (e :: SqlError)); return True)
  QM.assert result

--(e :: E.SomeException)

prop_RemoveTestDatabase :: Bool
prop_RemoveTestDatabase = True

main :: IO ()
main = do
  (host, user, password, database) <- SQL.confGetPostgresqlConfiguration
  let env = PGConfig host user password database
  TR.runReaderT createTestDatabase env
  Async.withAsync (W.run 8080 A.app) $ \a1 ->
    do
      --Q.quickCheckWith a prop_CreateTestDatabase
      --Q.quickCheckWith a prop_RemoveTestDatabase
      Async.cancel a1
      return ()
  where
    a =
      Q.Args
        { Q.replay = Q.replay Q.stdArgs,
          Q.maxSuccess = 1,
          Q.maxDiscardRatio = Q.maxDiscardRatio Q.stdArgs,
          Q.maxSize = Q.maxSize Q.stdArgs,
          Q.chatty = Q.chatty Q.stdArgs,
          Q.maxShrinks = Q.maxShrinks Q.stdArgs
        }

createTestDatabase :: TR.ReaderT PGConfig (IO) ()
createTestDatabase = do
  PGConfig host user password database <- ask
  let database = "testing_news_server_database"

  conn1 <- liftIO $ PGS.connect (ConnectInfo host 5432 user password "")
  _ <- liftIO $ PGS.execute_ conn1 $ fromString $ "CREATE DATABASE " ++ database ++ ";"
  PGS.close conn1

  query_text <- liftIO $ readFile $ "src" </> "SQL" </> "migrations" </> "init" </> "init.sql"
  conn2 <- liftIO $ PGS.connect (ConnectInfo host 5432 user password database)

  _ <- liftIO $ PGS.execute_ conn2 (fromString query_text)
  PGS.close conn2

  liftIO $ updateNewsDb
  liftIO $ addAdmin

  return ()
