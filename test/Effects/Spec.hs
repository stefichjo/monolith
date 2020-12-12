{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, GADTs #-}

module Effects.Spec where

import Test.Hspec

import Polysemy

import Effects.Mtl.Spec
import Effects.Sem.Spec

import Effects.A_Model
import Effects.B_Domain
import Effects.B_Language
import Effects.Fixtures

specEffects :: Spec
specEffects = do

  describe "app mock" $ do
    it "ok" $ do
      (app :: AppMock Event) `shouldBe` return expectedUser

  describe "app mock (sem)" $ do
    it "ok" $ do
      runMock appSem `shouldBe` return expectedUser

  specMtl -- more sophisticated test

instance Console AppMock where

  consoleRead = return consoleMock
  consoleWrite msg = return ()

instance DB AppMock where

  dbCreate user = return ()
  dbRead = return dbMock

instance Log AppMock where

  logWrite msg = return ()


runMock :: Sem '[ConsoleSem, DbSem, LogSem, Embed AppMock] Event -> AppMock Event
runMock =
  runM
    .
      (interpret $ \case
        LogSemWrite msg -> return ())
    .
      (interpret $ \case
        DbSemCreate user -> return ()
        DbSemRead        -> return dbMock)
    .
      (interpret $ \case
        ConsoleSemWrite line -> return ()
        ConsoleSemRead       -> return consoleMock)