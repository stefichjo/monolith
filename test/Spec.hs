module Main where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "my ad-hoc test" $ do
    it "should confirm that 2 + 2 == 4" $ do
      2 + 2 `shouldBe` 4