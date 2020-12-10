module Main where

import Test.Hspec
import Test.QuickCheck
import Effects.Spec

import Effects
import Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  specEffects

  describe "append" $ do
    it "should append" $ property $
      \x -> append x "Hi" `shouldBe` "Hi" <> [x]      

{-
Tagles Final:
https://serokell.io/blog/tagless-final
https://jproyo.github.io/posts/2019-03-17-tagless-final-haskell.html

Polysemy:
https://hackage.haskell.org/package/polysemy

Clean Architecture:
https://github.com/thma/PolysemyCleanArchitecture

Events:
https://blog.jayway.com/2013/06/20/dont-publish-domain-events-return-them/
-}

-- TODO Event instead of ()
-- REFACTOR JSON

-- TODO move to submodules Log'/Log
-- TODO use DBT and LogT as well

