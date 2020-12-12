{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, GADTs #-}
module Effects.Spec where
import Effects.A_Model
import Effects.B_Domain
import Effects.B_Language
import Effects.Fixtures
import Effects.Mtl.Spec
import Effects.Sem.Spec

import Test.Hspec

import Polysemy

specEffects :: Spec
specEffects = do

  describe "app mock" $ do
    it "ok" $ do
      app `shouldBe` (return expectedUser :: AppMock Event)

  describe "app mock (sem)" $ do
    it "ok" $ do
      interpretMock appSem `shouldBe` (return expectedUser :: AppMock Event)

  specMtl

  specSem

instance Console AppMock where

  consoleRead = return consoleMock
  consoleWrite msg = return ()

instance DB AppMock where

  dbRead = return dbMock
  dbCreate user = return ()

instance Log AppMock where

  logWrite msg = return ()


interpretMock :: Monad m => Sem '[ConsoleSem, DbSem, LogSem, Embed m] Event -> m Event
interpretMock =
  runM
    .
      (interpret $ \case
        LogSemWrite msg -> return ())
    .
      (interpret $ \case
        DbSemRead        -> return dbMock
        DbSemCreate user -> return ())
    .
      (interpret $ \case
        ConsoleSemRead       -> return consoleMock
        ConsoleSemWrite line -> return ())