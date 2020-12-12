{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
module Effects.Sem.Spec where
import Effects.Sem
import Effects.Utils hiding (App, app)
import Effects.Fixtures
import Effects.A_Model
import Effects.B_Domain hiding (Console, DB, Log, consoleRead, consoleWrite, dbRead, dbCreate, nextUser, logWrite)
import qualified Effects.B_Domain (Console, DB, Log, consoleRead, consoleWrite, dbRead, dbCreate, nextUser, logWrite)

import Test.Hspec
import Test.QuickCheck

import Utils
import Polysemy

specSem :: Spec
specSem = do

  describe "app" $ do
    it "can" $ do
      runMock app `shouldBe` return expectedUser

runMock :: Sem '[Console, DB, Log, Embed AppMock] Event -> AppMock Event
runMock =
  runM
    .
      (interpret $ \case
        LogWrite msg -> return ())
    .
      (interpret $ \case
        DbCreate user -> return ()
        DbRead        -> return dbMock)
    .
      (interpret $ \case
        ConsoleWrite line -> return ()
        ConsoleRead       -> return consoleMock)

-- TODO mtl-like app instance
-- TODO run with state, writer and reader