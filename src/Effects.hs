{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, DataKinds #-}

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

class DB m where

  dbCreate :: User -> m ()
  dbRead :: m [User]

  dbCreateNextUser :: Monad m => String -> m ()
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
data User =
  
  User {
    userId :: UserId,
    userName :: UserName
  }
  deriving (
    Eq, Ord, Show, Read)

{-
data LogDsl m a where

  LogWriteDsl :: String -> LogDsl m ()
instance Log (LogDsl m) where

  logWrite = LogWriteDsl

data DbDsl m a where

  DbCreateDsl :: User -> DbDsl m ()
  DbReadDsl :: DbDsl m [User]
instance DB (DbDsl m) where

  dbCreate = DbCreateDsl
  dbRead = DbReadDsl

data ConsoleDsl m a where

  ConsoleReadDsl :: ConsoleDsl m String
  ConsoleWriteDsl :: String -> ConsoleDsl m ()
instance Console (ConsoleDsl m) where

  consoleRead = ConsoleReadDsl
  consoleWrite = ConsoleWriteDsl
-}