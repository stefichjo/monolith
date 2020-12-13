module Utils.Spec where

import Test.Hspec ( Spec, describe, it, shouldBe )
import Test.QuickCheck ( Testable(property) )

import Utils ( append )

specUtils :: Spec
specUtils = describe "Utils" $ do
  it "appends" $ property $
    \x -> append x "Hi" `shouldBe` "Hi" <> [x]
