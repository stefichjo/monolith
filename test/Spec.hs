module Main where

import Test.Hspec ( Spec, hspec )

import Utils.Spec ( specUtils )
import Effects.Spec ( specEffects )
import Reservations.Spec ( specReservations )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  specUtils   

  specEffects

  specReservations
