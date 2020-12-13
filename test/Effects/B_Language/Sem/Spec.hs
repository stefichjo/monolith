{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
module Effects.B_Language.Sem.Spec where
import Effects.B_Language ()
import Effects.B_Domain ()
import Effects.A_Model ()

import Test.Hspec ( Spec, describe, it, shouldBe )

import Polysemy

-- TODO IMPLEMENT (PolysemyCleanArchitecture)
specSem :: Spec
specSem = do

  describe "app (sem)" $ do
    it "ok" $ do
      True `shouldBe` True

-- trace ?
-- 


equals :: Eq alpha => alpha -> alpha -> Bool
equals = (==)
