{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, GADTs #-}
module Effects.B_Language.Spec where
import Effects.B_Language.Mtl.Spec ( specMtl )
import Effects.B_Language.Sem.Spec ( specSem )
import Effects.B_Language ( Event, app', appSem )
import Effects.B_Domain
    ( Console(..),
      DB(dbRead, dbCreate),
      Log(..),
      ConsoleSem(..),
      DbSem(..),
      LogSem(..) )

import Effects.Fixtures ( consoleMock, dbMock, expectedUser )

import Test.Hspec ( Spec, describe, it, shouldBe )

import Control.Monad.Identity ( Identity )
import Polysemy ( Sem, runM, interpret, Embed )

specLanguage :: Spec
specLanguage = describe "B_Language" $ do

  it "returns expected user (monad mock)" $
    (app' :: AppMock Event) `shouldBe` return expectedUser

  it "returns expected user (Sem mock)" $
    (interpretMock appSem :: AppMock Event) `shouldBe` return expectedUser

  specMtl

  specSem

type AppMock = Identity

instance Console AppMock where

  consoleRead = return consoleMock

instance DB AppMock where

  dbRead = return dbMock
  dbCreate _ = return ()

instance Log AppMock where

  logWrite _ = return ()


interpretMock :: Monad m => Sem '[ConsoleSem, DbSem, LogSem, Embed m] Event -> m Event
interpretMock =
  runM
    .
      interpret (\case
        LogSemWrite _ -> return ())
    .
      interpret (\case
        DbSemRead     -> return dbMock
        DbSemCreate _ -> return ())
    .
      interpret (\case
        ConsoleSemRead    -> return consoleMock)
