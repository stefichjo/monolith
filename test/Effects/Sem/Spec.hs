module Effects.Sem.Spec where

import Test.Hspec
import Test.QuickCheck

import Utils
import Effects

specSem :: Spec
specSem = do

  describe "app" $ do
    it "can" $ do
      appMock `shouldBe` return (User {userId = 43, userName = "10"})
