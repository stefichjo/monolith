{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
module Effects.Sem.Spec where
import Effects.Sem
import Effects.Fixtures
import Effects.A_Model

import Test.Hspec

import Utils
import Polysemy

specSem :: Spec
specSem = do

  describe "app" $ do
    it "can" $ do
      runMock appSem `shouldBe` return expectedUser

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

-- TODO mtl-like app instance
-- TODO run with state, writer and reader