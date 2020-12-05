{-# LANGUAGE RankNTypes #-}

module Effects where

import FileSystem
import Utils
import Polysemy

type App m a =
  Log m =>
  DB m =>
  Console m =>
  Monad m =>
  m a

app :: App m ()
app = do
  consoleWrite "Yes?"
  name <- consoleRead
  logWrite $ "New user: " <> name <> "."
  dbCreateNextUser name
  consoleWrite "Bye!"

class Log m where

  logWrite :: String -> m ()

instance Log IO where

  logWrite = addFile logFileName

class Monad m => DB m where

  dbCreate :: User -> m ()
  dbRead :: m [User]

  dbCreateNextUser :: String -> m ()
  dbCreateNextUser name = do
    User lastId _ <- maximum <$> dbRead
    dbCreate (User (succ lastId) name)

instance DB IO where

  dbCreate = addFile dbFileName
  dbRead = map read . lines <$> readFileContents dbFileName

class Console m where

  consoleRead :: m String
  consoleWrite :: String -> m ()

instance Console IO where

  consoleRead = getLine
  consoleWrite = putStrLn

logFileName = "log" :: FilePath
dbFileName = "db" :: FilePath

type UserId = Int
type UserName = String
data User = User {
    userId :: UserId,
    userName :: UserName
  }
  deriving (Eq, Ord, Show, Read)

