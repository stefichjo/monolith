module Utils.Spec where

import Test.Hspec ( Spec, describe, it, shouldBe )
import Test.QuickCheck ( Testable(property) )

import Utils ( append )

specUtils :: Spec
specUtils = do

  describe "append" $ do
    it "should append" $ property $
      \x -> append x "Hi" `shouldBe` "Hi" <> [x]
