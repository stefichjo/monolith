{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
module Effects.Sem.Spec where
import Effects.Fixtures
import Effects.A_Model
import Effects.B_Domain
import Effects.B_Language

import Test.Hspec

import Utils
import Polysemy

-- TODO IMPLEMENT
specSem :: Spec
specSem = do

  describe "app (sem) -- to be implemented" $ do
    it "ok" $ do
      True `shouldBe` True