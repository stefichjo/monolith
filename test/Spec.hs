module Main where

import Test.Hspec

import Utils.Spec
import Effects.Spec

import Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  specUtils   

  specEffects

