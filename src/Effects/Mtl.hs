{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Effects.Mtl where
import Effects.Utils

import FileSystem
import Utils (append)

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

class Console m where

  consoleRead :: m String
  consoleWrite :: String -> m ()
class DB m where

  dbCreate :: User -> m ()
  dbRead :: m [User]

  nextUser :: Monad m => UserName -> m User
  nextUser name = do
    User lastId _ <- maximum <$> dbRead
    return $ User (succ lastId) name
class Log m where

  logWrite :: String -> m ()

instance Console AppMock where
  consoleRead = return consoleConst
  consoleWrite msg = return ()
instance DB AppMock where
  dbCreate user = return ()
  dbRead = return $ read inMemoryDbRaw
instance Log AppMock where
  logWrite msg = return ()

type App m a = (Console m, DB m, Log m, Monad m) => m a
instance Console IO where

  consoleRead = getLine
  consoleWrite = putStrLn
instance DB IO where

  dbCreate = addFile dbFileName
  dbRead = map read . lines <$> readFileContents dbFileName
instance Log IO where

  logWrite = addFile logFileName

app :: App m Event
app = do
  consoleWrite "Yes?"
  name <- consoleRead
  logWrite $ "New user: " <> name <> "."
  user <- nextUser name
  dbCreate user
  consoleWrite "Bye!"
  return user

mainMock :: AppMock Event
mainMock = app

mainIO :: IO ()
mainIO = app >>= print

type ConsoleT = WriterT String (Reader String)
instance Console ConsoleT where

  consoleRead = ask
  consoleWrite = tell
newtype ConsoleMtl a =

  ConsoleMtl {
    runConsoleMtl :: ConsoleT a }
  deriving (
    Functor, Applicative, Monad,
    MonadReader String, MonadWriter String)
instance Console ConsoleMtl where

  consoleRead = ConsoleMtl $ consoleRead
  consoleWrite msg = ConsoleMtl $ consoleWrite msg

type DBT = State [User]
instance DB DBT where

  dbCreate user = dbRead >>= put . append user
  dbRead = get
newtype DBMtl a =
  
  DBMtl {
    runDBMtl :: DBT a }
  deriving (
    Functor, Applicative, Monad,
    MonadState [User])
instance DB DBMtl where

  dbCreate user = DBMtl $ dbCreate user
  dbRead = DBMtl $ get

type LogT = Writer String
instance Log LogT where

  logWrite = tell
newtype LogMtl a =
  
  LogMtl {
    runLogMtl :: LogT a }
  deriving (
    Functor, Applicative, Monad,
    MonadWriter String)
instance Log LogMtl where

  logWrite msg = LogMtl $ logWrite msg

type AppT = StateT [User] ConsoleT
newtype AppMtl a =

  AppMtl {
    runAppMtl :: AppT a }
  deriving (
    Functor, Applicative, Monad,
    MonadReader String, MonadWriter String, MonadState [User])
instance Log AppMtl where

  logWrite = tell
instance DB AppMtl where

  dbCreate user = dbRead >>= put . append user
  dbRead = get
instance Console AppMtl where

  consoleRead = ask
  consoleWrite = tell
