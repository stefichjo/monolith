{-# LANGUAGE TypeSynonymInstances #-}

module Effects.Spec where

import Test.Hspec
import Test.QuickCheck

import Effects.Mtl.Spec
import Effects.Sem.Spec

import Effects.A_Model
import Effects.B_Domain
import Effects.B_Language
import Effects.Fixtures

specEffects :: Spec
specEffects = do

  describe "app mock" $ do
    it "can" $ do
      (app :: AppMock Event) `shouldBe` return expectedUser

  specMtl

  specSem

instance Console AppMock where

  consoleRead = return consoleMock
  consoleWrite msg = return ()

instance DB AppMock where

  dbCreate user = return ()
  dbRead = return dbMock

instance Log AppMock where

  logWrite msg = return ()
