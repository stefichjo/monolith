{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
module Effects.Sem.Spec where
import Effects.Sem
import Effects.Utils hiding (Console, DB, Log, App, app, consoleRead, consoleWrite, dbRead, dbCreate, nextUser, logWrite)
import qualified Effects.Utils (Console, DB, Log, App, app, consoleRead, consoleWrite, dbRead, dbCreate, nextUser, logWrite)
import Effects.Fixtures

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