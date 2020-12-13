module Reservations.Spec where
import Reservations

import Test.Hspec ( Spec, describe, it, shouldBe )

-- import Date.Time.Calendar

specReservations :: Spec
specReservations = describe "Domain Logic" $ do
  it "computes the used capacity for an empty list of reservations" $
    usedCapacity [] `shouldBe` 0

  it "computes the used capacity for a list of reservations" $
    usedCapacity [res1, res2] `shouldBe` 7

  it "computes the available seats for a list of reservations" $
    availableSeats totalCapacity [res1, res2] `shouldBe` 13

-- >>> day
-- 2020-01-29
