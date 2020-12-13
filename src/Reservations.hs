{-# LANGUAGE DeriveGeneric #-}
module Reservations where

import Data.Time.Calendar ( Day )
import Numeric.Natural ( Natural )
import GHC.Generics ( Generic )

-- A_MODEL ("The Domain layer")

-- | a data type representing a reservation
data Reservation = Reservation
    { date     :: Day    -- ^ the date of the reservation
    , name     :: String -- ^ the name of the guest placing the reservation
    , email    :: String -- ^ the email address of the guest
    , quantity :: Natural    -- ^ how many seats are requested
    }
    deriving (Eq, Generic, Read, Show)

reservation :: Reservation
reservation = Reservation {name = "Mr. Miller", quantity = 2, date = read "2020-06-01", email = "manfred@miller.com"}

