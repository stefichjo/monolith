{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TypeFamilies #-}
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

---- Paweł Szulc

data ConsoleDsl m a where

  PrintLineDsl :: String -> ConsoleDsl m ()
  ReadLineDsl :: ConsoleDsl m String
data RandomDsl v m a where

  NextRandomDsl :: RandomDsl v m v
data LogDsl m a where

  LogWriteDsl :: String -> LogDsl m ()

makeSem ''ConsoleDsl
makeSem ''RandomDsl
makeSem ''LogDsl

type Builder r a = Sem r a
type With dsl r = forall a. Builder (dsl ': r) a -> Builder r a
type Build m a = Monad m => Builder '[Embed m] a -> m a
type EmbedIO r = '[Embed IO] `Members` r
type App' r a = '[ConsoleDsl, RandomDsl Int, LogDsl] `Members` r => Builder r a

app' :: App' r Int
app' = do
  printLineDsl "Insert your number:"
  i1 <- readLineDsl
  i2 <- nextRandomDsl
  logWriteDsl $ "Adding " <> show i1 <> " and " <> show i2
  pure (read i1 + i2)

build :: Build m a
build = runM

withConsoleIO :: EmbedIO r => With ConsoleDsl r
withConsoleIO = interpret $ \case
  PrintLineDsl line -> embed (putStrLn line)
  ReadLineDsl       -> embed getLine
withRandomIO :: EmbedIO r => With (RandomDsl Int) r
withRandomIO = interpret $ \case
  NextRandomDsl -> embed randomIO
withLogIO :: EmbedIO r => With LogDsl r
withLogIO = interpret $ \case
  LogWriteDsl msg -> embed (addFile logFileName msg)

appIO :: IO Int
appIO = app'
  & withLogIO
  & withConsoleIO
  & withRandomIO
  & build

consoleConst = "10" :: String
randomConst = 20 :: Int

withConsole :: With ConsoleDsl r
withConsole = interpret $ \case
  PrintLineDsl line -> pure ()
  ReadLineDsl -> pure consoleConst
withRandom :: With (RandomDsl Int) r
withRandom = interpret $ \case
  NextRandomDsl -> pure randomConst
withLog :: With LogDsl r
withLog = interpret $ \case
  LogWriteDsl msg -> pure ()

appConst :: Monad m => m Int
appConst = app'
  & withConsole
  & withRandom
  & withLog
  & build

-- generalize: `withConsole`, `withRandom`
-- type family?

main' :: IO ()
main' = appIO >>= putStrLn . show

{-
class GMapKey k where
  data GMap k :: * -> *
  empty       :: GMap k v
  lookup      :: k -> GMap k v -> Maybe v
  insert      :: k -> v -> GMap k v -> GMap k v

instance GMapKey Int where
  data GMap Int v        = GMapInt (Data.IntMap.IntMap v)
  empty                  = GMapInt Data.IntMap.empty
  lookup k   (GMapInt m) = Data.IntMap.lookup k m
  insert k v (GMapInt m) = GMapInt (Data.IntMap.insert k v m)

instance GMapKey () where
  data GMap () v           = GMapUnit (Maybe v)
  empty                    = GMapUnit Nothing
  lookup () (GMapUnit v)   = v
  insert () v (GMapUnit _) = GMapUnit $ Just v
-}
