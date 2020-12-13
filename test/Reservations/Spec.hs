module Reservations.Spec where
import Reservations

import Test.Hspec ( Spec, describe, it, shouldBe )

-- import Date.Time.Calendar

specReservations :: Spec
specReservations = describe "Domain Logic" $ do
  it "computes the used capacity for an empty list of reservations" $
    0 `shouldBe` 0

-- >>> day
-- 2020-01-29
