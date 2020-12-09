{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Effects (
  module Polysemy,
  module Effects
) where

import FileSystem
import Utils
import Polysemy
import Data.Function ((&))
import System.Random (randomIO)
import qualified Data.IntMap

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity

type UserId = Int
type UserName = String
data User =
  
  User {
    userId :: UserId,
    userName :: UserName
  }
  deriving (
    Eq, Ord, Show, Read)

-- TODO Event instead of ()
-- REFACTOR JSON

-- TODO move to submodules Log'/Log
-- Split here: Tagless Final

class Log' m where

  logWrite' :: String -> m ()
class Console' m where

  consoleRead' :: m String
  consoleWrite' :: String -> m ()
class DB' m where

  dbCreate' :: User -> m ()
  dbRead' :: m [User]

  dbCreateNextUser' :: Monad m => String -> m ()
  dbCreateNextUser' name = do
    User lastId _ <- maximum <$> dbRead'
    dbCreate' (User (succ lastId) name)

type App' m a = (Monad m, Log' m, Console' m, DB' m) => m a
instance Log' IO where

  logWrite' = addFile logFileName
instance Console' IO where

  consoleRead' = getLine
  consoleWrite' = putStrLn
instance DB' IO where

  dbCreate' = addFile dbFileName
  dbRead' = map read . lines <$> readFileContents dbFileName

type AppMock = Identity
instance Log' AppMock where
  logWrite' msg = pure ()
instance Console' AppMock where
  consoleRead' = pure consoleConst
  consoleWrite' msg = pure ()
instance DB' AppMock where
  dbCreate' user = pure ()
  dbRead' = pure $ read inMemoryDbRaw

mainMock :: AppMock ()
mainMock = app'

app' :: App' m ()
app' = do
  consoleWrite' "Yes?"
  name <- consoleRead'
  logWrite' $ "New user: " <> name <> "."
  dbCreateNextUser' name
  consoleWrite' "Bye!"

-- Split here: Polysemy

data Log m a where {

    LogWrite :: String -> Log m ();
  
  }; makeSem ''Log
data Console m a where {

    ConsoleWrite :: String -> Console m ();
    ConsoleRead :: Console m String;

  }; makeSem ''Console
data DB m a where {

    DbCreate :: User -> DB m ();
    DbRead :: DB m [User];

  }; makeSem ''DB

dbCreateNextUser :: String -> App r ()
dbCreateNextUser name = do
  User lastId  _ <- maximum <$> dbRead
  dbCreate (User (succ lastId) name)

type App r a = '[Console, DB, Log] `Members` r => Builder r a

app :: App r ()
app = do
  consoleWrite "Yes?"
  name <- consoleRead
  logWrite $ "New user: " <> name <> "."
  dbCreateNextUser name
  consoleWrite "Bye!"

-- REFACTOR generalize: `app`, `withLog`, ...

appIO :: IO ()
appIO = app
  & (interpret $ \case
      LogWrite msg -> embed (addFile logFileName msg))
  & (interpret $ \case
      ConsoleWrite line -> embed (putStrLn line)
      ConsoleRead       -> embed getLine)
  & (interpret $ \case
      DbCreate user -> embed . addFile dbFileName $ user
      DbRead -> embed (map read . lines <$> readFileContents dbFileName))
  & build

appM :: Monad m => m ()
appM = app
  & (interpret $ \case
      LogWrite msg -> return ())
  & (interpret $ \case
      ConsoleWrite line -> return ()
      ConsoleRead       -> return consoleConst)
  & (interpret $ \case
      DbCreate user -> return ()
      DbRead        -> return $ read inMemoryDbRaw)
  & build

-- TODO just for fun: App' instances of App
-- TODO instance Log' (* -> *) where

-- instance Log' ("App") where

-- TODO type for (* -> *)
-- instance Log' (App r) where

type LogT = Writer String
instance Log' LogT where

  logWrite' = tell
newtype LogMtl a =
  
  LogMtl {
    runLogMtl :: LogT a }
  deriving (
    Functor, Applicative, Monad,
    MonadWriter String)
instance Log' LogMtl where

  logWrite' msg = LogMtl $ logWrite' msg

type ConsoleT = WriterT String (Reader String)
instance Console' ConsoleT where

  consoleRead' = ask
  consoleWrite' = tell
newtype ConsoleMtl a =

  ConsoleMtl {
    runConsoleMtl :: ConsoleT a }
  deriving (
    Functor, Applicative, Monad,
    MonadReader String, MonadWriter String)
instance Console' ConsoleMtl where

  consoleRead' = ConsoleMtl $ consoleRead'
  consoleWrite' msg = ConsoleMtl $ consoleWrite' msg

type DBT = State [User]
instance DB' DBT where

  dbCreate' user = dbRead' >>= put . append user
  dbRead' = get
newtype DBMtl a =
  
  DBMtl {
    runDBMtl :: DBT a }
  deriving (
    Functor, Applicative, Monad,
    MonadState [User])
instance DB' DBMtl where

  dbCreate' user = DBMtl $ dbCreate' user
  dbRead' = DBMtl $ get

-- TODO use DBT and LogT as well

type AppT = StateT [User] ConsoleT
newtype AppMtl a =

  AppMtl {
    runAppMtl :: AppT a }
  deriving (
    Functor, Applicative, Monad,
    MonadReader String, MonadWriter String, MonadState [User])
instance Log' AppMtl where

  logWrite' = tell
instance DB' AppMtl where

  dbCreate' user = dbRead' >>= put . append user
  dbRead' = get
instance Console' AppMtl where

  consoleRead' = ask
  consoleWrite' = tell

