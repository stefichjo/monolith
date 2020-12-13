{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, GADTs #-}
module Effects.B_Language.Spec where
import Effects.B_Language.Mtl.Spec
import Effects.B_Language.Sem.Spec
import Effects.B_Language
import Effects.B_Domain
import Effects.A_Model

import Effects.Fixtures

import Test.Hspec

import Control.Monad.Identity
import Polysemy

specB_Language :: Spec
specB_Language = do

  describe "app mock (monad)" $ do
    it "ok" $ do
      (app :: AppMock Event) `shouldBe` return expectedUser

  describe "app mock (sem)" $ do
    it "ok" $ do
      (interpretMock appSem :: AppMock Event) `shouldBe` return expectedUser

  specMtl

  specSem

type AppMock = Identity

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