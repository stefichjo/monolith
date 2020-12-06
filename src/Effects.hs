{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
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
  PrintLine :: String -> ConsoleDsl m ()
  ReadLine :: ConsoleDsl m String
data RandomDsl v m a where
  NextRandom :: RandomDsl v m v

makeSem ''ConsoleDsl
makeSem ''RandomDsl

type ProgramBuilder r a = [ConsoleDsl, RandomDsl a] `Members` r => Sem r a
type With dsl r = forall a. Sem (dsl ': r) a -> Sem r a

withConsoleIO ::
     Member (Embed IO) r
  => With ConsoleDsl r
withConsoleIO = interpret $ \case
  PrintLine line -> embed (putStrLn line)
  ReadLine       -> embed getLine

withConsoleConst ::
     String
  -> With ConsoleDsl r
withConsoleConst constLine = interpret $ \case
  PrintLine line -> pure ()
  ReadLine -> pure constLine

withRandomIO ::
     Member (Embed IO) r
  => With (RandomDsl Int) r
withRandomIO = interpret $ \case
  NextRandom -> embed randomIO

withRandomConst ::
     Int
  -> With (RandomDsl Int) r
withRandomConst v = interpret $ \case
  NextRandom -> pure v


programBuilder :: ProgramBuilder r Int
programBuilder = do
  printLine "Insert your number:"
  i1 <- readLine
  i2 <- nextRandom
  pure (read i1 + i2)

programM :: IO Int
programM = programBuilder
  & withConsoleIO
  & withRandomIO
  & runM

programConst :: Monad m => m Int
programConst = programBuilder
  & withConsole
  & withRandom
  & runM

withConsole :: With ConsoleDsl r
withConsole = withConsoleConst "10"

withRandom :: With (RandomDsl Int) r
withRandom = withRandomConst 20

main' :: IO ()
main' = programM >>= putStrLn . show

-- generalize: `withConsole`, `withRandom`

-- class Foo m where
--   data FooData :: * -> *
--   withConsole :: FooData Char

-- IDE
-- jump to definition
-- Hoogle

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

