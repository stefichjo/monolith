{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

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

class Log' m where

  logWrite :: String -> m ()
instance Log' IO where

  logWrite = addFile logFileName
class Console' m where

  consoleRead :: m String
  consoleWrite :: String -> m ()
instance Console' IO where

  consoleRead = getLine
  consoleWrite = putStrLn
class DB' m where

  dbCreate :: User -> m ()
  dbRead :: m [User]

  dbCreateNextUser :: Monad m => String -> m ()
  dbCreateNextUser name = do
    User lastId _ <- maximum <$> dbRead
    dbCreate (User (succ lastId) name)
instance DB' IO where

  dbCreate = addFile dbFileName
  dbRead = map read . lines <$> readFileContents dbFileName

type App' m a = (Monad m, Log' m, Console' m, DB' m) => m a

app :: App' m ()
app = do
  consoleWrite "Yes?"
  name <- consoleRead
  logWrite $ "New user: " <> name <> "."
  dbCreateNextUser name
  consoleWrite "Bye!"

data Log m a where {

    LogWriteDsl :: String -> Log m ();
  
  }; makeSem ''Log
withLogIO :: WithIO Log r
withLogIO = interpret $ \case
  LogWriteDsl msg -> embed (addFile logFileName msg)
data Console m a where {

    ConsoleWriteDsl :: String -> Console m ();
    ConsoleReadDsl :: Console m String;

  }; makeSem ''Console
withConsoleIO :: WithIO Console r
withConsoleIO = interpret $ \case
  ConsoleWriteDsl line -> embed (putStrLn line)
  ConsoleReadDsl       -> embed getLine
data DB m a where {

    DbCreateDsl :: User -> DB m ();
    DbReadDsl :: DB m [User];

  }; makeSem ''DB
withDbIO :: WithIO DB r
withDbIO = interpret $ \case
  DbCreateDsl user -> embed . addFile dbFileName $ user
  DbReadDsl -> embed (map read . lines <$> readFileContents dbFileName)
dbCreateNextUserDsl :: String -> App r ()
dbCreateNextUserDsl name = do
  User lastId  _ <- maximum <$> dbReadDsl
  dbCreateDsl (User (succ lastId) name)

type App r a = '[Console, DB, Log] `Members` r => Builder r a

appDsl :: App r ()
appDsl = do
  consoleWriteDsl "Yes?"
  name <- consoleReadDsl
  logWriteDsl $ "New user: " <> name <> "."
  dbCreateNextUserDsl name
  consoleWriteDsl "Bye!"

appIO :: IO ()
appIO = appDsl
  & withLogIO
  & withConsoleIO
  & withDbIO
  & build

withLog :: With Log r
withLog = interpret $ \case
  LogWriteDsl msg -> pure ()
withConsole :: With Console r
withConsole = interpret $ \case
  ConsoleWriteDsl line -> pure ()
  ConsoleReadDsl -> pure consoleConst
withDb :: With DB r
withDb = interpret $ \case
  DbCreateDsl user -> pure ()
  DbReadDsl -> pure inMemoryDB

appConst :: Monad m => m ()
appConst = appDsl
  & withLog
  & withConsole
  & withDb
  & build

-- REFACTOR generalize: `withConsole`, `withRandom`

main' :: IO ()
main' = appIO >>= putStrLn . show

inMemoryDB = read inMemoryDbRaw :: [User]
