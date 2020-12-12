{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
module Effects.Sem.Spec where
import Effects.Sem
import Effects.Fixtures
import Effects.A_Model
import Effects.B_Domain hiding (Console, DB, Log, consoleRead, consoleWrite, dbRead, dbCreate, nextUser, logWrite)
import qualified Effects.B_Domain (Console, DB, Log, consoleRead, consoleWrite, dbRead, dbCreate, nextUser, logWrite)

import Test.Hspec

import Utils
import Polysemy

specSem :: Spec
specSem = do

  describe "app" $ do
    it "can" $ do
      runMock app `shouldBe` return expectedUser

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