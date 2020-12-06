{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Effects (
  module Polysemy,
  module Effects
) where

import FileSystem
import Utils
import Polysemy
import Data.Function ((&))
import System.Random (randomIO)

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



data ConsoleDsl m a where
  PrintLine :: String -> ConsoleDsl m ()
  ReadLine :: ConsoleDsl m String
data RandomDsl v m a where
  NextRandom :: RandomDsl v m v

makeSem ''ConsoleDsl
makeSem ''RandomDsl

withConsoleIO ::
     Member (Embed IO) r
  => Sem (ConsoleDsl ': r) a -> Sem r a
withConsoleIO = interpret $ \case
  PrintLine line -> embed (putStrLn line)
  ReadLine       -> embed getLine

withRandomIO ::
     Member (Embed IO) r
  => Sem (RandomDsl Int ': r) a -> Sem r a
withRandomIO = interpret $ \case
  NextRandom -> embed randomIO

runConsoleConst :: String -> Sem (ConsoleDsl ': r) a -> Sem r a
runConsoleConst constLine = interpret $ \case
  PrintLine line -> pure ()
  ReadLine -> pure constLine

runRandomConst :: Int -> Sem (RandomDsl Int ': r) a -> Sem r a
runRandomConst v = interpret $ \case
  NextRandom -> pure v

type ProgramBuilder r a =
     Member ConsoleDsl r
  => Member (RandomDsl a) r
  => Sem r a

programBuilder :: ProgramBuilder r Int
programBuilder = do
  printLine "Insert your number:"
  i1 <- readLine
  i2 <- nextRandom
  pure (read i1 + i2)

main' :: IO ()
main' = programM >>= putStrLn . show

programM :: IO Int
programM = programBuilder
  & withConsoleIO
  & withRandomIO
  & runM

program :: Int
program = programBuilder
  & runConsoleConst "10"
  & runRandomConst 20
  & run

-- generalize: `withConsole`, `withRandom`, `build`

-- IDE
-- jump to definition
-- Hoogle