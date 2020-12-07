{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

type UserId = Int
type UserName = String
data User =
  
  User {
    userId :: UserId,
    userName :: UserName
  }
  deriving (
    Eq, Ord, Show, Read)

-- TODO move to submodules Log'/Log

class Log' m where

  logWrite' :: String -> m ()
instance Log' IO where

  logWrite' = addFile logFileName
class Console' m where

  consoleRead' :: m String
  consoleWrite' :: String -> m ()
instance Console' IO where

  consoleRead' = getLine
  consoleWrite' = putStrLn
class DB' m where

  dbCreate' :: User -> m ()
  dbRead' :: m [User]

  dbCreateNextUser' :: Monad m => String -> m ()
  dbCreateNextUser' name = do
    User lastId _ <- maximum <$> dbRead'
    dbCreate' (User (succ lastId) name)
instance DB' IO where

  dbCreate' = addFile dbFileName
  dbRead' = map read . lines <$> readFileContents dbFileName

type App' m a = (Monad m, Log' m, Console' m, DB' m) => m a

app' :: App' m ()
app' = do
  consoleWrite' "Yes?"
  name <- consoleRead'
  logWrite' $ "New user: " <> name <> "."
  dbCreateNextUser' name
  consoleWrite' "Bye!"

data Log m a where {

    LogWrite :: String -> Log m ();
  
  }; makeSem ''Log
withLogIO :: WithIO Log r
withLogIO = interpret $ \case
  LogWrite msg -> embed (addFile logFileName msg)
data Console m a where {

    ConsoleWrite :: String -> Console m ();
    ConsoleRead :: Console m String;

  }; makeSem ''Console
withConsoleIO :: WithIO Console r
withConsoleIO = interpret $ \case
  ConsoleWrite line -> embed (putStrLn line)
  ConsoleRead       -> embed getLine
data DB m a where {

    DbCreate :: User -> DB m ();
    DbRead :: DB m [User];

  }; makeSem ''DB
withDbIO :: WithIO DB r
withDbIO = interpret $ \case
  DbCreate user -> embed . addFile dbFileName $ user
  DbRead -> embed (map read . lines <$> readFileContents dbFileName)
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

appIO :: IO ()
appIO = app
  & withLogIO
  & withConsoleIO
  & withDbIO
  & build

withLog :: With Log r
withLog = interpret $ \case
  LogWrite msg -> pure ()
withConsole :: With Console r
withConsole = interpret $ \case
  ConsoleWrite line -> pure ()
  ConsoleRead -> pure consoleConst
withDb :: With DB r
withDb = interpret $ \case
  DbCreate user -> pure ()
  DbRead -> pure inMemoryDB

appConst :: Monad m => m ()
appConst = app
  & withLog
  & withConsole
  & withDb
  & build

-- REFACTOR generalize: `withConsole`, `withRandom`

main' :: IO ()
main' = appIO >>= putStrLn . show

inMemoryDB = read inMemoryDbRaw :: [User]

-- TODO just for fun: App' instances of App
-- TODO instance Log' (* -> *) where

-- instance Log' ("App") where

-- TODO type for (* -> *)
-- instance Log' (App r) where

