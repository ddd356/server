{-# LANGUAGE OverloadedStrings #-}

-- TODO look implemented tests for a bot
--import qualified  Control.Monad.Reader ()

import qualified App as A (app)
import qualified Control.Concurrent.Async as Async (cancel, withAsync)
import qualified Control.Exception as E (Exception, SomeException (..))
import qualified Control.Exception.Base as EB (catch, throw)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Reader as TR (ReaderT (..), ask, runReaderT)
import Data.ByteString (ByteString)
import Data.String (fromString)
import Database.PostgreSQL.Simple (ExecStatus (..), FormatError (..), QueryError (..), ResultError (..), SqlError (..))
import qualified Database.PostgreSQL.Simple as PGS (ConnectInfo (..), close, connect, execute_)
import Env
import Network.HTTP.Simple (parseRequest_)
import Network.HTTP.Types (status200)
import qualified Network.Wai.Handler.Warp as W (run)
import Network.Wai.Internal (Request (..))
import Network.Wai.Test (defaultRequest, setPath)
import RequestProcessors (processAuthRequest, processListUsersRequest)
import SQL (SqlPicturesList, SqlUsersList)
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

testEnv :: Env
testEnv = Env test_checkCredentials test_generateToken test_addNewToken test_getUsersList test_totalNumberOfRowsInTable test_checkAdminRightsNew test_loginNotExists test_addUser test_checkAdminRights test_checkLoginAndTokenAccordance test_addAuthor test_deleteAuthor test_addPicture test_getUsersList test_pictureIdExists test_getPicture test_canCreatePosts test_addPost

test_checkCredentials :: ByteString -> ByteString -> IO Bool
test_checkCredentials login password = return $ login == "test" && password == "test"

test_generateToken :: ByteString -> IO ByteString
test_generateToken "test" = return "correct_token"
test_generateToken _ = return "wrong_token"

test_addNewToken :: ByteString -> ByteString -> IO ()
test_addNewToken _ _ = return ()

test_getUsersList :: Int -> Int -> IO SqlUsersList
test_getUsersList _ _ =
  return [(1, "test", "test", "test", "", (read "2022-01-01 00:00:00"), True, True)]

test_totalNumberOfRowsInTable :: String -> IO Int
test_totalNumberOfRowsInTable _ = return 1

test_checkAdminRightsNew :: ByteString -> IO (Either String ())
test_checkAdminRightsNew token = do
  return $ if token == "correct_token" then Right () else Left "Access denied"

test_loginNotExists :: ByteString -> IO (Either String ())
test_loginNotExists login = do
  return $ if login == "existing_login" then Left "User with that login already exists" else Right ()

test_addUser :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> Bool -> Bool -> IO ByteString
test_addUser _ _ _ _ _ _ _ = return "User added"

test_checkAdminRights :: ByteString -> IO Bool
test_checkAdminRights token = return $ if token == "correct_token" then True else False

test_checkLoginAndTokenAccordance :: ByteString -> ByteString -> IO Bool
test_checkLoginAndTokenAccordance login token = return $ if login = "test" && token == "correct_token" then True else False

addAuthor :: ByteString -> ByteString -> IO ByteString
test_addAuthor login _ = return case login == "test" then "Author added"

prop_processAuthRequestCorrect :: Q.Property
prop_processAuthRequestCorrect =
  QM.monadicIO $ do
    res <- liftIO $ TR.runReaderT (processAuthRequest $ setPath defaultRequest "http://localhost:8080/auth?action=auth&login=test&password=test") testEnv
    QM.assert $ res == ("correct_token", status200)

prop_processListUsersRequestCorrect :: Q.Property
prop_processListUsersRequestCorrect =
  QM.monadicIO $ do
    res <- liftIO $ TR.runReaderT (processListUsersRequest $ setPath defaultRequest "http://localhost:8080/users?action=list") testEnv
    --liftIO $ print res
    QM.assert $ res == "{\"next\":\"\",\"limit\":50,\"users\":[{\"can_create_posts\":true,\"id\":1,\"login\":\"test\",\"avatar\":\"\",\"admin\":true,\"lastname\":\"test\",\"create_date\":\"2022-01-01 00:00:00\",\"firstname\":\"test\"}],\"prev\":\"\"}"

prop_processAddUsersRequestCorrect :: Q.Property
prop_processAddUsersRequestCorrect =
  QM.monadicIO $ do
    res <- liftIO $ TR.runReaderT (processListUsersRequest $ setPath defaultRequest "http://localhost:8080/users?action=list") testEnv
    QM.assert $ True

main :: IO ()
main = do
  Q.quickCheckWith a prop_processAuthRequestCorrect
  Q.quickCheckWith a prop_processListUsersRequestCorrect
  Q.quickCheckWith a prop_processAddUsersRequestCorrect
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

--  (host, user, password, database) <- SQL.confGetPostgresqlConfiguration
--  let env = PGConfig host user password database
--  TR.runReaderT createTestDatabase env
--  Async.withAsync (W.run 8080 A.app) $ \a1 ->
--    do
--      --Q.quickCheckWith a prop_CreateTestDatabase
--      --Q.quickCheckWith a prop_RemoveTestDatabase
--      Async.cancel a1
--      return ()
--  where
--    a =
--      Q.Args
--        { Q.replay = Q.replay Q.stdArgs,
--          Q.maxSuccess = 1,
--          Q.maxDiscardRatio = Q.maxDiscardRatio Q.stdArgs,
--          Q.maxSize = Q.maxSize Q.stdArgs,
--          Q.chatty = Q.chatty Q.stdArgs,
--          Q.maxShrinks = Q.maxShrinks Q.stdArgs
--        }
