{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Effects.Mtl.Spec where
import Effects.Fixtures
import Effects.A_Model
import Effects.B_Domain
import Effects.B_Language

import Test.Hspec

import Utils

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

specMtl :: Spec
specMtl = do

  describe "app (mtl)" $ do
    it "ok" $ do
      runApp (app :: AppMtl Event) dbMock consoleMock
        `shouldBe`
          (,)
            (expectedUser, (dbMock <> [expectedUser]))
            "Yes?New user: Fizz.Bye!"

  specOK

  where

    appT = runAppMtl app
    
    runApp app = runReader . runWriterT . runStateT appT

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

  dbRead = get
  dbCreate user = dbRead >>= put . append user

newtype DBMtl a =
  
  DBMtl {
    runDBMtl :: DBT a }
  deriving (
    Functor, Applicative, Monad,
    MonadState [User])

instance DB DBMtl where

  dbRead = DBMtl $ get
  dbCreate user = DBMtl $ dbCreate user

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

  dbRead = get
  dbCreate user = dbRead >>= put . append user

instance Console AppMtl where

  consoleRead = ask
  consoleWrite = tell

-- can this be simplified, intermediate steps omitted?
-- are app and appmock tested in sem spec?

specOK :: Spec
specOK = do

  describe "ok" $ do
    it "should be ok" $ do
      and [okLog, okDB, okConsole] `shouldBe` True

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
  runDBT (dbCreate (last dbMock)) (init dbMock)
  ==
  ((), dbMock),
  
  -- DBT [User]
  runDBT dbRead dbMock
  ==
  (dbMock, dbMock),

  -- DBMtl ()
  runDB (dbCreate (last dbMock)) (init dbMock)
  ==
  ((), dbMock),
  
  -- DBMtl [User]
  runDB dbRead dbMock
  ==
  (dbMock, dbMock),

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
