{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
module Effects.B_Language.Sem.Spec where
import Effects.B_Language ()
import Effects.B_Domain ()
import Effects.A_Model ()

import Test.Hspec ( Spec, describe, it, shouldBe )

-- TODO IMPLEMENT (PolysemyCleanArchitecture)
specSem :: Spec
specSem = do

  describe "app (sem)" $ do
    it "ok" $ do
      True `shouldBe` True
