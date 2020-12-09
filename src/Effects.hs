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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

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

-- TODO User instead of ()

type Event = User

app' :: App' m Event
app' = do
  consoleWrite' "Yes?"
  name <- consoleRead'
  logWrite' $ "New user: " <> name <> "."
  dbCreateNextUser' name
  consoleWrite' "Bye!"
  return $ User 42 "sts"

mainMock' :: AppMock Event
mainMock' = app'

mainIO' :: IO ()
mainIO' = app' >>= print

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

type App r a = Members '[Console, DB, Log] r => Sem r a

app :: Members '[Console, DB, Log] r => Sem r Event
app = do
  consoleWrite "Yes?"
  name <- consoleRead
  logWrite $ "New user: " <> name <> "."
  dbCreateNextUser name
  consoleWrite "Bye!"
  return $ User 42 "sts"

type AppIO = IO

runIO :: Sem '[DB, Console, Log, Embed AppIO] Event -> AppIO Event
runIO =
  runM
    .
      (interpret $ \case
        LogWrite msg -> embed $ addFile logFileName msg)
    . 
      (interpret $ \case
        ConsoleWrite line -> embed $ putStrLn line
        ConsoleRead       -> embed getLine)
    .
      (interpret $ \case
        DbCreate user -> embed $ addFile dbFileName $ user
        DbRead        -> embed $ map read . lines <$> readFileContents dbFileName)
mainIO :: IO ()
mainIO = runIO app >>= print

runMock :: Sem '[DB, Console, Log, Embed AppMock] Event -> AppMock Event
runMock =
  runM
    .
      (interpret $ \case
        LogWrite msg -> return ())
    .
      (interpret $ \case
        ConsoleWrite line -> return ()
        ConsoleRead       -> return consoleConst)
    .
      (interpret $ \case
        DbCreate user -> return ()
        DbRead        -> return $ read inMemoryDbRaw)
appMock :: AppMock Event
appMock = runMock app
mainMock :: IO ()
mainMock = appMock & print

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
