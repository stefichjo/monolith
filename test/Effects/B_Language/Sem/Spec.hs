{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
module Effects.B_Language.Sem.Spec where
import Effects.B_Language
import Effects.B_Domain
import Effects.A_Model
import Effects.Fixtures

import Test.Hspec

import Utils
import Polysemy

-- TODO IMPLEMENT
specSem :: Spec
specSem = do

  describe "app (sem)" $ do
    it "ok" $ do
      True `shouldBe` True