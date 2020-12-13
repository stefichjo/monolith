module Reservations.Spec where
import Reservations

import Test.Hspec ( Spec, describe, it, shouldBe )

specReservations :: Spec
specReservations = do

  describe "app mock (monad)" $ do
    it "ok" $ do
      True `shouldBe` True

