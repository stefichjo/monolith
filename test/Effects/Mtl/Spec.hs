{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Effects.Mtl.Spec where
import Effects.A_Model
import Effects.B_Domain
import Effects.B_Language
import Effects.Fixtures

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

type DBT = State [User]
instance DB DBT where

  dbRead = get
  dbCreate user = dbRead >>= put . append user

type LogT = Writer String
instance Log LogT where

  logWrite = tell

type AppT = StateT [User] ConsoleT
newtype AppMtl a =

  AppMtl {
    runAppMtl :: AppT a }
  deriving (
    Functor, Applicative, Monad,
    MonadReader String, MonadWriter String, MonadState [User])

instance Console AppMtl where

  consoleRead = ask
  consoleWrite = tell

instance DB AppMtl where

  dbRead = get
  dbCreate user = dbRead >>= put . append user

instance Log AppMtl where

  logWrite = tell

specOK :: Spec
specOK = do

  describe "ok" $ do
    it "should be ok" $ do
      and [okLog, okDB, okConsole] `shouldBe` True

okConsole = and [

  (runReader . runWriterT) (consoleRead) "Hi!"
  ==
  ("Hi!", ""),
  
  (runReader . runWriterT) (consoleWrite "Hi!") ""
  ==
  ((), "Hi!"),

  True]
  
okDB = and [

  runState (dbCreate (last dbMock)) (init dbMock)
  ==
  ((), dbMock),
  
  runState dbRead dbMock
  ==
  (dbMock, dbMock),

  True]

okLog = and [

    runWriter (logWrite "Hi!")
    ==
    ((), "Hi!"),
    
  True]
