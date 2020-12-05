{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Effects (
    module Effects,
    module Control.Monad.Reader,
    module Control.Monad.Writer,
    module Control.Monad.State
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import FileSystem

import Polysemy

-- TODO Atom Editor
-- TODO Member, Eff
-- TODO DSL (GADT)

{-
withdraw :: (
  Member Bank r,
  Member Logger r)
  => Int
  -> Eff r (Maybe Int)
-}

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

type LogT = Writer String
instance Log LogT where

  logWrite = tell

newtype LogMtl a = LogMtl { runLogMtl :: LogT a }
  deriving (Functor, Applicative, Monad, MonadWriter String)
instance Log LogMtl where

  logWrite msg = LogMtl $ logWrite msg

instance Log AppMtl where

  logWrite = tell

type DBT = State [User]
instance DB DBT where

  dbCreate user = dbRead >>= put . append user
  dbRead = get

newtype DBMtl a = DBMtl { runDBMtl :: DBT a }
  deriving (Functor, Applicative, Monad, MonadState [User])
instance DB DBMtl where

  dbCreate user = DBMtl $ dbCreate user
  dbRead = DBMtl $ get

instance DB AppMtl where

  dbCreate user = dbRead >>= put . append user
  dbRead = get

type ConsoleT = WriterT String (Reader String)
instance Console ConsoleT where

  consoleRead = ask
  consoleWrite = tell

newtype ConsoleMtl a =
  ConsoleMtl {
    runConsoleMtl :: ConsoleT a }
  deriving (Functor, Applicative, Monad, MonadReader String, MonadWriter String)
instance Console ConsoleMtl where

  consoleRead = ConsoleMtl $ consoleRead
  consoleWrite msg = ConsoleMtl $ consoleWrite msg

instance Console AppMtl where

  consoleRead = ask
  consoleWrite = tell

type AppT = StateT [User] ConsoleT
newtype AppMtl a =
  AppMtl {
    runAppMtl :: AppT a }
  deriving (Functor, Applicative, Monad, MonadReader String, MonadWriter String, MonadState [User])

logFileName = "log" :: FilePath
dbFileName = "db" :: FilePath

inMemoryDB = [
    User 42 "Bar",
    User 23 "Foo"
  ]

type UserId = Int
type UserName = String
data User = User { userId :: UserId, userName :: UserName }
  deriving (Eq, Ord, Show, Read)
append :: a -> [a] -> [a]
append = (flip (<>)) . pure
