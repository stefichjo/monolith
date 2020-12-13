{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
module Effects.B_Language.Sem.Spec where
import Effects.B_Language ()
import Effects.B_Domain ()
import Effects.A_Model ()

import Test.Hspec ( Spec, describe, it, shouldBe )

-- TODO IMPLEMENT (PolysemyCleanArchitecture)
specSem :: Spec
specSem = describe "B_Language.Sem" $ do

  it "runs with expected in-memory effects" $
    True `shouldBe` True
