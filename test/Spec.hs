module Main where

import Test.Hspec ( Spec, hspec )

import Utils.Spec ( specUtils )
import Effects.Spec ( specEffects )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  specUtils   

  specEffects
