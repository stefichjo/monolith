{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
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

instance Console IO where

  consoleRead = getLine
  consoleWrite = putStrLn
instance DB IO where

  dbCreate = addFile dbFileName
  dbRead = map read . lines <$> readFileContents dbFileName
instance Log IO where

  logWrite = addFile logFileName

type App m a = (Console m, DB m, Log m, Monad m) => m a
app :: App m Event
app = do
  consoleWrite "Yes?"
  name <- consoleRead
  logWrite $ "New user: " <> name <> "."
  user <- nextUser name
  dbCreate user
  consoleWrite "Bye!"
  return user

mainIO :: IO ()
mainIO = app >>= print

