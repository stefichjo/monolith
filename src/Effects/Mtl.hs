{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
module Effects.Mtl where

import FileSystem
import Utils

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity

class Log' m where

  logWrite' :: String -> m ()
class Console' m where

  consoleRead' :: m String
  consoleWrite' :: String -> m ()
class DB' m where

  dbCreate' :: User -> m ()
  dbRead' :: m [User]

  nextUser' :: Monad m => UserName -> m User
  nextUser' name = do
    User lastId _ <- maximum <$> dbRead'
    return $ User (succ lastId) name

type AppMock = Identity
instance Log' AppMock where
  logWrite' msg = return ()
instance Console' AppMock where
  consoleRead' = return consoleConst
  consoleWrite' msg = return ()
instance DB' AppMock where
  dbCreate' user = return ()
  dbRead' = return $ read inMemoryDbRaw

type App' m a = (Monad m, Log' m, Console' m, DB' m) => m a
instance Log' IO where

  logWrite' = addFile logFileName
instance Console' IO where

  consoleRead' = getLine
  consoleWrite' = putStrLn
instance DB' IO where

  dbCreate' = addFile dbFileName
  dbRead' = map read . lines <$> readFileContents dbFileName

type Event = User

app' :: App' m Event
app' = do
  consoleWrite' "Yes?"
  name <- consoleRead'
  logWrite' $ "New user: " <> name <> "."
  user <- nextUser' name
  dbCreate' user
  consoleWrite' "Bye!"
  return user

mainMock' :: AppMock Event
mainMock' = app'

mainIO' :: IO ()
mainIO' = app' >>= print

