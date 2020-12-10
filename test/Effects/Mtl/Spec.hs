{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Effects.Mtl.Spec where
import Effects.Mtl
import Effects.Utils
import Effects.Fixtures

import Test.Hspec
import Test.QuickCheck

import Utils

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

inMemoryDB = read inMemoryDbRaw

specMtl :: Spec
specMtl = do

  describe "app" $ do
    it "can" $ do
      (app :: AppMock Event) `shouldBe` return (User {userId = 43, userName = "10"})

  describe "ok" $ do
    it "should be ok" $ do
      and [okLog, okDB, okConsole] `shouldBe` True

okApp = and [

    -- AppMtl ()
    runApp app inMemoryDB "sts"
    ==
    (,)
      (User {userId = 42, userName = "sts"}, (inMemoryDB <> [User 42 "sts"]))
      "Yes?New user: Fizz.Bye!",
      
  True]

runApp app db = runReader (runWriterT (runStateT (runAppMtl app) db))


okLog = and [

    -- LogT ()
    runLogT (logWrite "Hi!")
    ==
    ((), "Hi!"),
    
    -- LogMtl ()
    runLog (logWrite "Hi!")
    ==
    ((), "Hi!"),
    
  True] where

  runLogT = runWriter
  runLog = runLogT . runLogMtl
okDB = and [

  -- DBT ()
  runDBT (dbCreate (last inMemoryDB)) (init inMemoryDB)
  ==
  ((), inMemoryDB),
  
  -- DBT [User]
  runDBT dbRead inMemoryDB
  ==
  (inMemoryDB, inMemoryDB),

  -- DBMtl ()
  runDB (dbCreate (last inMemoryDB)) (init inMemoryDB)
  ==
  ((), inMemoryDB),
  
  -- DBMtl [User]
  runDB dbRead inMemoryDB
  ==
  (inMemoryDB, inMemoryDB),

  True] where

  runDBT = runState
  runDB = runDBT . runDBMtl
okConsole = and [

  -- ConsoleT String
  runConsoleT (consoleRead) "Hi!"
  ==
  ("Hi!", ""),
  
  -- ConsoleT ()
  runConsoleT (consoleWrite "Hi!") ""
  ==
  ((), "Hi!"),
  
  
  -- ConsoleMtl String
  runConsole (consoleRead) "Hi!"
  ==
  ("Hi!", ""),

  -- ConsoleMtl ()
  runConsole (consoleWrite "Hi!") ""
  ==
  ((), "Hi!"),
  
  True] where
  
  runConsoleT = runReader . runWriterT
  runConsole = runConsoleT . runConsoleMtl

instance Console AppMock where
  consoleRead = return consoleConst
  consoleWrite msg = return ()
instance DB AppMock where
  dbCreate user = return ()
  dbRead = return $ read inMemoryDbRaw
instance Log AppMock where
  logWrite msg = return ()

type ConsoleT = WriterT String (Reader String)
instance Console ConsoleT where

  consoleRead = ask
  consoleWrite = tell
newtype ConsoleMtl a =

  ConsoleMtl {
    runConsoleMtl :: ConsoleT a }
  deriving (
    Functor, Applicative, Monad,
    MonadReader String, MonadWriter String)
instance Console ConsoleMtl where

  consoleRead = ConsoleMtl $ consoleRead
  consoleWrite msg = ConsoleMtl $ consoleWrite msg

type DBT = State [User]
instance DB DBT where

  dbCreate user = dbRead >>= put . append user
  dbRead = get
newtype DBMtl a =
  
  DBMtl {
    runDBMtl :: DBT a }
  deriving (
    Functor, Applicative, Monad,
    MonadState [User])
instance DB DBMtl where

  dbCreate user = DBMtl $ dbCreate user
  dbRead = DBMtl $ get

type LogT = Writer String
instance Log LogT where

  logWrite = tell
newtype LogMtl a =
  
  LogMtl {
    runLogMtl :: LogT a }
  deriving (
    Functor, Applicative, Monad,
    MonadWriter String)
instance Log LogMtl where

  logWrite msg = LogMtl $ logWrite msg

type AppT = StateT [User] ConsoleT
newtype AppMtl a =

  AppMtl {
    runAppMtl :: AppT a }
  deriving (
    Functor, Applicative, Monad,
    MonadReader String, MonadWriter String, MonadState [User])
instance Log AppMtl where

  logWrite = tell
instance DB AppMtl where

  dbCreate user = dbRead >>= put . append user
  dbRead = get
instance Console AppMtl where

  consoleRead = ask
  consoleWrite = tell
