{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, GADTs #-}
module Effects.B_Language.Spec where
import Effects.B_Language.Mtl.Spec ( specMtl )
import Effects.B_Language.Sem.Spec ( specSem )
import Effects.B_Language ( app, appSem )
import Effects.B_Domain
    ( Console(..),
      DB(dbRead, dbCreate),
      Log(..),
      ConsoleSem(..),
      DbSem(..),
      LogSem(..) )
import Effects.A_Model ( Event )

import Effects.Fixtures ( consoleMock, dbMock, expectedUser )

import Test.Hspec ( Spec, describe, it, shouldBe )

import Control.Monad.Identity ( Identity )
import Polysemy ( Sem, runM, interpret, Embed )

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
  consoleWrite _ = return ()

instance DB AppMock where

  dbRead = return dbMock
  dbCreate _ = return ()

instance Log AppMock where

  logWrite _ = return ()


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