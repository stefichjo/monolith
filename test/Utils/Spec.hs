module Utils.Spec where

import Test.Hspec
import Test.QuickCheck

import Utils

specUtils :: Spec
specUtils = do

  describe "append" $ do
    it "should append" $ property $
      \x -> append x "Hi" `shouldBe` "Hi" <> [x]   