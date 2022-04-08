{-# LANGUAGE OverloadedStrings #-}

module SQL where

import Crypto.PBKDF (sha512PBKDF2)
import Data.Aeson (Value (..), encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (append, null)
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8 (readInt)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.UTF8 (toString)
import qualified Data.Configurator as Cfg (Worth (..), autoConfig, autoReload, require)
import Data.Either (rights)
import Data.List (foldl', intercalate)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.String (fromString)
import Data.Time.LocalTime (LocalTime (..), ZonedTime (..), getZonedTime)
import Database.PostgreSQL.Simple (Binary (..), ConnectInfo (..), close, connect, execute, execute_, query, query_)
import Database.PostgreSQL.Simple.Time (parseLocalTime)
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..), toField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import qualified Database.PostgreSQL.Simple.Types as PG (Null (..))
import Log.Handle
import Log.Impl.BotLog as Log
import Random (getRandomString)
import System.FilePath ((</>))

type SqlUsersList = [(Int, String, String, String, ByteString, LocalTime, Bool, Bool)]

type SqlPicturesList = [(Int, ByteString)]

type SqlCategoriesList = [(Int, ByteString)]

type Token = ByteString

data FilteringParameters = FilteringParameters
  { created_at :: Maybe LocalTime,
    created_until :: Maybe LocalTime,
    created_since :: Maybe LocalTime,
    author :: ByteString,
    category_id :: Int,
    name_contains :: ByteString,
    text_contains :: ByteString,
    search :: ByteString
  }

data PaginationParameters = PaginationParameters
  { limit :: Int,
    from :: Int
  }

newtype SortingParameters = SortingParameters
  { sort_by :: SortingField
  }
  deriving (Show)

data SortingField = SortByDate | SortByAuthor | SortByCategory | SortByPicturesQuantity | SortById deriving (Eq, Show)

instance ToRow FilteringParameters where
  toRow fp =
    [toField p1 | isJust p1]
      ++ [toField p2 | isJust p2]
      ++ [toField p3 | isJust p3]
      ++ (if BS.null p4 then [] else toField p4 : [toField p4]) -- two same wildcards
      ++ [toField p5 | p5 /= 0]
      ++ [toField $ ilike p6 | not (BS.null p6)]
      ++ [toField $ ilike p7 | not (BS.null p7)]
      ++ (if BS.null p8 then [] else toField p8 : toField p8 : [toField p8])
    where
      p1 = created_at fp
      p2 = created_until fp
      p3 = created_since fp
      p4 = author fp
      p5 = category_id fp
      p6 = name_contains fp
      p7 = text_contains fp
      p8 = search fp
      ilike x = "%" `BS.append` x `BS.append` "%"

instance ToRow PaginationParameters where
  toRow pp = [toField p1, toField p2]
    where
      p1 = if limit pp > 50 || limit pp < 1 then 50 else limit pp
      p2 = from pp

instance ToRow SortingParameters where
  toRow sp = [toField p1]
    where
      p1 = sort_by sp

instance ToField SortingField where
  toField SortByDate = Plain (byteString "create_date")
  toField SortByAuthor = Plain (byteString "author")
  toField SortByCategory = Plain (byteString "category")
  toField SortByPicturesQuantity = Plain (byteString "(SELECT count(*) FROM news_pictures WHERE news_id = news.id)")
  toField SortById = Plain (byteString "id") -- default

checkNewsDBExistense :: IO Bool
checkNewsDBExistense = do
  Log.log DEBUG "Checking news database existense"

  (host, user, password, _) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user password "")
  db_names <- query_ conn "SELECT datname FROM pg_database;" :: IO [[String]]
  database <- confGetPostgresqlDatabase
  close conn
  if [database] `elem` db_names
    then return True
    else
      ( do
          putStrLn "Database doesn't exist on a server. Run with -create-db to initialize database."
          return False
      )

confGetPostgresqlConfiguration :: IO (String, String, String, String)
confGetPostgresqlConfiguration = do
  Log.log DEBUG "Reading PostgreSQL configuration"

  host <- confGetPostgresqlHost
  user <- confGetPostgresqlUser
  password <- confGetPostgresqlPassword
  database <- confGetPostgresqlDatabase
  return (host, user, password, database)

confGetPostgresqlHost :: IO String
confGetPostgresqlHost = do
  (cfg, _) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]
  Cfg.require cfg "host"

confGetPostgresqlUser :: IO String
confGetPostgresqlUser = do
  (cfg, _) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]
  Cfg.require cfg "user"

confGetPostgresqlPassword :: IO String
confGetPostgresqlPassword = do
  (cfg, _) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]
  Cfg.require cfg "password"

confGetPostgresqlDatabase :: IO String
confGetPostgresqlDatabase = do
  (cfg, _) <- Cfg.autoReload Cfg.autoConfig [Cfg.Required "conf.cfg"]
  Cfg.require cfg "database"

-- UPDATING DB

updateNewsDb :: IO ()
updateNewsDb = do
  Log.log DEBUG "Updating database"

  res <- getVersion
  let version = head . head $ res
  sequence_ $ drop (version -1) migrations

migrations :: [IO ()]
migrations =
  [migration_v2]
    ++ [migration_v3]
    ++ [migration_v4]

-- ++ [migration_v...]

migration_v2 :: IO ()
migration_v2 = do
  Log.log DEBUG "Migration to version 2"

  (host, user, password, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user password database)

  -- add column "parent_id"
  query_text_1 <- readFile $ "src" </> "SQL" </> "migrations" </> "v2" </> "add_column_parent_id.sql"
  _ <- execute_ conn (fromString query_text_1)

  -- add constraint "parent_id_foreign"
  query_text_2 <- readFile $ "src" </> "SQL" </> "migrations" </> "v2" </> "add_constraint_parent_id_foreign.sql"
  _ <- execute_ conn (fromString query_text_2)

  -- raise version
  _ <- execute_ conn "UPDATE version SET version = 2"

  close conn
  return ()

migration_v3 :: IO ()
migration_v3 = do
  Log.log DEBUG "Migration to version 3"

  (host, user, password, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user password database)

  -- add function jsonb_cat_with_parents
  query_text_1 <- readFile $ "src" </> "SQL" </> "migrations" </> "v3" </> "add_function_jsonb_cat_with_parents.sql"
  _ <- execute_ conn (fromString query_text_1)

  -- add function jsonb_post_pictures
  query_text_2 <- readFile $ "src" </> "SQL" </> "migrations" </> "v3" </> "add_function_jsonb_post_pictures.sql"
  _ <- execute_ conn (fromString query_text_2)

  -- add function jsonb_post_tags
  query_text_3 <- readFile $ "src" </> "SQL" </> "migrations" </> "v3" </> "add_function_jsonb_post_tags.sql"
  _ <- execute_ conn (fromString query_text_3)

  -- add function uri_picture
  query_text_4 <- readFile $ "src" </> "SQL" </> "migrations" </> "v3" </> "add_function_uri_picture.sql"
  _ <- execute_ conn (fromString query_text_4)

  -- raise version
  _ <- execute_ conn "UPDATE version SET version = 3"
  close conn
  return ()

migration_v4 :: IO ()
migration_v4 = do
  Log.log DEBUG "Migration to version 4"

  (host, user, password, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user password database)

  -- add new column "can_create_posts" to users table
  query_text <- readFile $ "src" </> "SQL" </> "migrations" </> "v4" </> "add_field_can_create_posts.sql"
  _ <- execute_ conn (fromString query_text)

  --raise version
  _ <- execute_ conn "UPDATE version SET version = 4"
  close conn
  return ()

getVersion :: IO [[Int]]
getVersion = do
  (host, user, password, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user password database)
  res <- query_ conn "SELECT version FROM public.version;" :: IO [[Int]]
  close conn
  return res

-- CREATING DB

createNewsDb :: IO ()
createNewsDb = do
  Log.log DEBUG "Creating new database"

  (host, user, password, database) <- confGetPostgresqlConfiguration
  conn1 <- connect (ConnectInfo host 5432 user password "")
  _ <- execute_ conn1 $ fromString $ "CREATE DATABASE " ++ database ++ ";"
  close conn1

  query_text <- readFile $ "src" </> "SQL" </> "migrations" </> "init" </> "init.sql"
  conn2 <- connect (ConnectInfo host 5432 user password database)

  _ <- execute_ conn2 (fromString query_text)
  close conn2

  updateNewsDb
  addAdmin

  return ()

addAdmin :: IO ()
addAdmin = do
  Log.log DEBUG "Adding admin user"

  (host, user, password, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user password database)

  query_text <- readFile ("src" </> "SQL" </> "migrations" </> "init" </> "addAdmin.sql")

  salt <- getRandomString 10
  let hash = salt ++ sha512PBKDF2 "admin" salt 2 45

  zoned_time <- getZonedTime
  let create_date = zonedTimeToLocalTime zoned_time

  _ <- execute conn (fromString query_text) (hash, create_date)
  close conn
  return ()

-- USERS

getUsersList :: Int -> Int -> IO SqlUsersList
getUsersList frm lmt = do
  Log.log DEBUG "Getting user list"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  result <- query_ conn (fromString $ "SELECT id, firstname, lastname, login, CASE WHEN avatar IS NULL THEN '' ELSE avatar END, create_date, admin, can_create_posts FROM public.users ORDER BY id" ++ (if lmt > 0 then " LIMIT " ++ show lmt else "") ++ ("OFFSET " ++ show frm)) :: IO SqlUsersList
  close conn
  return result

addUser :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> Bool -> Bool -> IO ByteString
addUser firstname lastname avatar login password admin can_create_posts = do
  Log.log DEBUG "Adding new user"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)

  salt <- getRandomString 10
  let hash = salt ++ sha512PBKDF2 (toString password) salt 2 45

  zoned_time <- getZonedTime
  let create_date = zonedTimeToLocalTime zoned_time

  query_text <- readFile $ "src" </> "SQL" </> "users" </> "addUser.sql"
  _ <- execute conn (fromString query_text) (firstname, lastname, Binary avatar, login, hash, create_date, admin, can_create_posts)
  close conn
  return "User added"

loginNotExists :: ByteString -> IO (Either String ())
loginNotExists login = do
  Log.log DEBUG "Check login existense"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  res <- query conn "SELECT 1 FROM users WHERE login = ?" [login] :: IO [[Int]]
  close conn
  return $ case res of
    [] -> Right ()
    _ -> Left "User with that login already exists"

-- AUTHORS

deleteAuthor :: ByteString -> IO ByteString
deleteAuthor login = do
  Log.log DEBUG "Deleting author"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  _ <- execute conn "DELETE FROM public.authors WHERE user_id IN (SELECT id FROM public.users WHERE users.login = ?)" [login]
  close conn
  return "Author deleted"

addAuthor :: ByteString -> ByteString -> IO ByteString
addAuthor login description = do
  Log.log DEBUG "Adding new author"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  _ <-
    execute
      conn
      "INSERT INTO public.authors (description, user_id) \
      \ SELECT ?, id \
      \ FROM public.users \
      \ WHERE users.login = ?"
      (description, login)
  close conn
  return "Author added"

-- POSTS

addPost :: ByteString -> Int -> Int -> ByteString -> Int -> IO ()
addPost shortName athr category text mainPicture = do
  Log.log DEBUG "Adding a new post"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  query_text <- readFile $ "src" </> "SQL" </> "posts" </> "addPost.sql"
  let create_date = head . rights $ [parseLocalTime "2020-12-21 12:00:00"]
  _ <- execute conn (fromString query_text) (shortName, create_date, athr, category, text, mainPicture)
  close conn
  return ()

fromDraft :: Int -> IO ByteString
fromDraft postID = do
  Log.log DEBUG "Publishing post"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  query_text <- readFile $ "src" </> "SQL" </> "posts" </> "fromDraft.sql"
  _ <- execute conn (fromString query_text) [postID]
  close conn
  return "Post published from drafts"

toDraft :: Int -> IO ByteString
toDraft postID = do
  Log.log DEBUG "Moving post to draft"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  query_text <- readFile $ "src" </> "SQL" </> "posts" </> "toDraft.sql"
  _ <- execute conn (fromString query_text) [postID]
  close conn
  return "Post moved to drafts"

addTagToPost :: Int -> Int -> IO ByteString
addTagToPost postID tagID = do
  Log.log DEBUG "Adding tag to post"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  query_text <- readFile $ "src" </> "SQL" </> "posts" </> "addTagToPost.sql"
  _ <- execute conn (fromString query_text) (postID, tagID)
  close conn
  return "Tag added to post"

removeTagFromPost :: Int -> Int -> IO ByteString
removeTagFromPost postID tagID = do
  Log.log DEBUG "Removing tag from post"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  query_text <- readFile $ "src" </> "SQL" </> "posts" </> "removeTagFromPost.sql"
  _ <- execute conn (fromString query_text) (postID, tagID)
  close conn
  return "Tag removed from post"

addPictureToPost :: Int -> Int -> IO ByteString
addPictureToPost postID pictureID = do
  Log.log DEBUG "Adding picture to post"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  query_text <- readFile $ "src" </> "SQL" </> "posts" </> "addPictureToPost.sql"
  _ <- execute conn (fromString query_text) (postID, pictureID)
  close conn
  return "Picture added to post"

removePictureFromPost :: Int -> Int -> IO ByteString
removePictureFromPost postID pictureID = do
  Log.log DEBUG "Removing picture from post"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  query_text <- readFile $ "src" </> "SQL" </> "posts" </> "removePictureFromPost.sql"
  _ <- execute conn (fromString query_text) (postID, pictureID)
  close conn
  return "Picture removed from post"

getPostsList :: FilteringParameters -> SortingParameters -> PaginationParameters -> Token -> Bool -> IO ByteString
getPostsList fp sp pp token showDraft = do
  Log.log DEBUG "Gettin list of posts"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  template_1 <- readFile $ "src" </> "SQL" </> "posts" </> "getPostsList_1.sql"
  template_2 <- readFile $ "src" </> "SQL" </> "posts" </> "getPostsList_2.sql"

  let query_text =
        template_1
          -- sorting parameters
          -- ++ (if sort_by sp == SortByNothing then "" else " ORDER BY ?")
          -- pagination parameters
          -- ++ (if from pp == 0 then "" else " OFFSET ?")
          -- ++ (if limit pp == 0 then "" else " FETCH ?")
          -- filtering parameters
          ++ (if isNothing $ created_at fp then "" else " AND create_date = ?")
          ++ (if isNothing $ created_until fp then "" else " AND create_date <= ?")
          ++ (if isNothing $ created_since fp then "" else " AND create_date >= ?")
          ++ (if BS.null $ author fp then "" else " AND (subquery1.firstname = ? OR subquery1.lastname = ?)")
          ++ (if category_id fp == 0 then "" else " AND category = ?")
          ++ (if BS.null $ name_contains fp then "" else " AND short_name ILIKE ?")
          ++ (if BS.null $ text_contains fp then "" else " AND text ILIKE ?")
          ++ (if BS.null $ search fp then "" else " AND (subquery1.firstname ILIKE ? OR subquery1.lastname = ? OR short_name ILIKE ? OR text ILIKE ?)")
          ++ template_2

  res <- query conn (fromString query_text) ([toField token] ++ [toField showDraft] ++ toRow fp ++ toRow sp ++ toRow pp) :: IO [[Value]]
  let json = if null res then "[]" else toStrict . encode . head . head $ res

  close conn
  return json

updatePost :: Int -> [(String, ByteString)] -> IO ByteString
updatePost id' vals = do
  Log.log DEBUG "Updating post"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)

  let tmplt = (++ " = ?")
  let f (ns, vs) (n, v) = case n of
        "category" -> (tmplt n : ns, (if v == "null" then toField PG.Null else toField . bytestringToInt $ v) : vs)
        "main_picture" -> (tmplt n : ns, (if v == "null" then toField PG.Null else toField . bytestringToInt $ v) : vs)
        _ -> (tmplt n : ns, toField v : vs)
  let (pnames, pvals) = foldl' f ([], []) vals

  let set_text = intercalate ", " pnames
  let pvals' = pvals ++ [toField id']

  let query_text =
        "UPDATE news SET "
          ++ set_text
          ++ " WHERE id = ?"
  _ <- execute conn (fromString query_text) pvals'
  close conn
  return "Post updated"

testQ :: IO ()
testQ = do
  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  res <- query conn (fromString "SELECT ? + ?") ([2, 2] :: [Int]) :: IO [[Int]]
  print res

-- PICTURES

addPicture :: ByteString -> IO ByteString
addPicture picture = do
  Log.log DEBUG "Adding new picture"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  _ <- execute conn "INSERT INTO public.pictures (picture) VALUES (?)" [Binary picture]
  close conn
  return "Picture added"

getPicturesList :: Int -> Int -> IO SqlPicturesList
getPicturesList frm lmt = do
  Log.log DEBUG "Getting list of pictures"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  result <-
    query_
      conn
      ( fromString $
          "SELECT id, picture FROM public.pictures ORDER BY id"
            ++ (if lmt > 0 then " LIMIT " ++ show lmt else "")
            ++ ("OFFSET " ++ show frm)
      ) ::
      IO SqlPicturesList
  close conn
  return result

getPicture :: Int -> IO ByteString
getPicture picture_id = do
  Log.log DEBUG "Getting a picture"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  query_text <- readFile $ "src" </> "SQL" </> "pictures" </> "getPicture.sql"
  result <- query conn (fromString query_text) [picture_id] :: IO [[ByteString]]
  close conn
  if null result then return "" else return $ head . head $ result

pictureIdExists :: Int -> IO (Either String ())
pictureIdExists picture_id = do
  Log.log DEBUG "Checking picture ID exists"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  res <- query conn "SELECT id FROM pictures WHERE id = ?" [picture_id] :: IO [[Int]]
  close conn
  return $ case res of
    [] -> Left "Picture with that ID don't exist"
    _ -> Right ()

-- CATEGORIES

addCategory :: ByteString -> Int -> IO ByteString
addCategory name parent_id = do
  Log.log DEBUG "Adding new category"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  _ <- case parent_id of
    0 -> execute conn "INSERT INTO public.categories (name) VALUES (?)" [name]
    _ -> execute conn "INSERT INTO public.categories (name, parent_id) VALUES (?, ?)" (name, parent_id)
  close conn
  return "Category added"

getCategoriesList :: PaginationParameters -> IO ByteString
getCategoriesList pp = do
  Log.log DEBUG "Getting category list"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  query_text <- readFile $ "src" </> "SQL" </> "categories" </> "getCategoriesList.sql"

  res <- query conn (fromString query_text) (toRow pp) :: IO [[Value]]
  let json = if null res then "[]" else toStrict . encode . head . head $ res

  close conn
  return json

updateCategory :: Int -> [(String, ByteString)] -> IO ByteString
updateCategory cat_id vals = do
  Log.log DEBUG "Updating category"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)

  let tmplt = (++ " = ?")
  let f (ns, vs) (n, v) = case n of
        "parent_id" -> (tmplt n : ns, (if v == "null" then toField PG.Null else toField . bytestringToInt $ v) : vs)
        _ -> (tmplt n : ns, toField v : vs)
  let (pnames, pvals) = foldl' f ([], []) vals

  let set_text = intercalate ", " pnames
  let pvals' = pvals ++ [toField cat_id]

  let query_text =
        "UPDATE categories SET "
          ++ set_text
          ++ " WHERE id = ?"
  _ <- execute conn (fromString query_text) pvals'
  close conn
  return "Category updated"

categoryIdExists :: Int -> IO (Either String ())
categoryIdExists cat_id = do
  Log.log DEBUG "Checking category ID exists"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  res <- query conn "SELECT id FROM categories WHERE id = ?" [cat_id] :: IO [[Int]]
  close conn
  return $ case res of
    [] -> Left "Category with that id don't exist"
    _ -> Right ()

-- TAGS

addTag :: ByteString -> IO ByteString
addTag name = do
  Log.log DEBUG "Adding a new tag"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  _ <- execute conn "INSERT INTO public.tags (name) VALUES (?)" [name]
  close conn
  return "Tag added"

-- CHECK CREDENTIALS

checkCredentials :: ByteString -> ByteString -> IO Bool
checkCredentials login password = do
  Log.log DEBUG "Checking credentials"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  --let hash = salt ++ (sha512PBKDF2 (toString password) salt 2 45)
  result <- query conn "SELECT password FROM users WHERE login = ? LIMIT 1" [login] :: IO [[String]]
  close conn
  return $ case result of
    [[hash]] -> sha512PBKDF2 (toString password) (take 10 hash) 2 45 == drop 10 hash
    _ -> False

checkLoginAndTokenAccordance :: ByteString -> ByteString -> IO Bool
checkLoginAndTokenAccordance login token = do
  Log.log DEBUG "Checking login and token accordance"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  result <- query conn "SELECT tokens.user_id FROM public.tokens INNER JOIN public.users ON users.id = tokens.user_id WHERE tokens.token = ? AND users.login = ?" (token, login) :: IO [[Int]]
  close conn
  return . not . null $ result

checkPostAndTokenAccordance :: Int -> ByteString -> IO Bool
checkPostAndTokenAccordance postID token = do
  Log.log DEBUG "Checking post and token accordance"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  query_text <- readFile $ "src" </> "SQL" </> "postAndTokenAccordance.sql"
  result <- query conn (fromString query_text) (postID, token) :: IO [[Int]]
  close conn
  return . not . null $ result

checkPostAndTokenAccordanceNew :: Int -> ByteString -> IO (Either String ())
checkPostAndTokenAccordanceNew postID token = do
  Log.log DEBUG "Checking post and token accordance"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  query_text <- readFile $ "src" </> "SQL" </> "postAndTokenAccordance.sql"
  result <- query conn (fromString query_text) (postID, token) :: IO [[Int]]
  close conn
  return $ if null result then Left "Access denied" else Right ()

generateToken :: ByteString -> IO ByteString
generateToken login = do
  Log.log DEBUG ("Generating new token for user " ++ toString login)

  -- read parameters from config
  (host, user, pass, database) <- confGetPostgresqlConfiguration

  -- generating random token
  newToken <- getRandomString 20

  -- open connection
  conn <- connect (ConnectInfo host 5432 user pass database)

  -- query for checking token existense
  res <- query conn "SELECT token FROM public.tokens WHERE token = ?" [newToken] :: IO [[String]]
  if not (null res)
    then generateToken login
    else return . fromString $ newToken

addNewToken :: ByteString -> ByteString -> IO ()
addNewToken login token = do
  Log.log DEBUG ("Adding new token for user " ++ show login)

  -- read parameters from config
  (host, user, pass, database) <- confGetPostgresqlConfiguration

  -- open connection
  conn <- connect (ConnectInfo host 5432 user pass database)

  query_text <- readFile $ "src" </> "SQL" </> "auth" </> "addNewToken.sql"
  _ <- execute conn (fromString query_text) (login, token, login)

  -- closing connection
  close conn

  return ()

checkAdminRights :: ByteString -> IO Bool
checkAdminRights token = do
  Log.log DEBUG "Checking admin rights"

  -- read parameters from config
  (host, user, pass, database) <- confGetPostgresqlConfiguration

  -- open connection
  conn <- connect (ConnectInfo host 5432 user pass database)

  -- query for checking admin rights
  res <- query conn "SELECT tokens.user_id FROM tokens INNER JOIN users ON tokens.user_id = users.id WHERE tokens.token = ? AND users.admin" [token] :: IO [[Int]]
  close conn
  return $ not (null res)

checkAdminRightsNew :: ByteString -> IO (Either String ())
checkAdminRightsNew token = do
  Log.log DEBUG "Checking admin rights"

  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)

  res <- query conn "SELECT 1 FROM tokens INNER JOIN users ON tokens.user_id = users.id WHERE tokens.token = ? AND users.admin" [token] :: IO [[Int]]
  close conn
  return $ if not (null res) then Right () else Left "Access denied"

canCreatePosts :: Token -> IO (Either String ())
canCreatePosts token = do
  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)

  res <- query conn "SELECT 1 FROM tokens INNER JOIN users ON tokens.user_id = users.id WHERE tokens.token = ? AND users.can_create_posts" [token] :: IO [[Int]]

  close conn
  return $ if not (null res) then Right () else Left "Access denied"

-- OTHER

totalNumberOfRowsInTable :: String -> IO Int
totalNumberOfRowsInTable table = do
  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  query_res <- query_ conn . fromString $ "SELECT count(*) FROM " ++ table ++ ";" :: IO [[Int]]
  let res = head . head $ query_res
  close conn
  return res

totalNumberOfPosts :: FilteringParameters -> IO Int
totalNumberOfPosts fp = do
  (host, user, pass, database) <- confGetPostgresqlConfiguration
  conn <- connect (ConnectInfo host 5432 user pass database)
  template <- readFile $ "src" </> "SQL" </> "posts" </> "totalNumberOfPosts.sql"
  let query_text =
        template
          ++ (if isNothing $ created_at fp then "" else " AND create_date = ?")
          ++ (if isNothing $ created_until fp then "" else " AND create_date <= ?")
          ++ (if isNothing $ created_since fp then "" else " AND create_date >= ?")
          ++ (if BS.null $ author fp then "" else " AND (subquery1.firstname ILIKE ? OR subquery1.lastname ILIKE ?)")
          ++ (if category_id fp == 0 then "" else " AND category = ?")
          ++ (if BS.null $ name_contains fp then "" else " AND short_name ILIKE ?")
          ++ (if BS.null $ text_contains fp then "" else " AND text ILIKE ?")
          ++ (if BS.null $ search fp then "" else " AND (subquery1.firstname ILIKE ? OR subquery1.lastname = ? OR short_name ILIKE ? OR text ILIKE ?)")
  query_res <- query conn (fromString query_text) (toRow fp) :: IO [[Int]]
  let res = head . head $ query_res
  close conn
  return res

bytestringToInt :: ByteString -> Int
bytestringToInt = fst . fromJust . readInt
