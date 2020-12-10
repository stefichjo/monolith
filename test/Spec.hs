module Main where

import Test.Hspec
import Test.QuickCheck

import Utils.Spec
import Effects.Spec

import Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  specUtils   

  specEffects

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

-- REFACTOR JSON

-- TODO use DBT and LogT as well

