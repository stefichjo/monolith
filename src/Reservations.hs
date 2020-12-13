{-# LANGUAGE DeriveGeneric #-}
module Reservations where

import Data.Time.Calendar (fromGregorian,  Day )
import Numeric.Natural ( Natural )
import GHC.Generics ( Generic )
import Data.Map (fromList,  Map )

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

-- | a key value map holding a list of reservations for any given day
type ReservationMap = Map Day [Reservation]

foo :: ReservationMap
foo = fromList
  [
    (
      read "2020-06-01",
        [
          Reservation {date = read "2020-06-01", name = "Mr. Miller", email = "manfred@miller.com", quantity = 2}, 
          Reservation {date = read "2020-06-01", name = "Andrew M. Jones", email = "amjones@example.com", quantity = 4}
        ]
    )
  ]

-- | computes the number of reserved seats for a list of reservations
usedCapacity :: [Reservation] -> Natural
usedCapacity = foldr ((+) . quantity) 0

-- | computes the number of available seats from a maximum capacity and a list of reservations.
availableSeats :: Natural-> [Reservation] -> Natural
availableSeats maxCapacity reservations = maxCapacity - usedCapacity reservations

day :: Day
day = fromGregorian 2020 1 29
res1 :: Reservation
res1 = Reservation day "Andrew M. Jones" "amjones@example.com" 4
res2 :: Reservation
res2 = Reservation day "Thomas Miller" "tm@example.com" 3
reservations :: [Reservation]
reservations = [res1, res2]
totalCapacity :: Integer
totalCapacity = 20

-- >>> day
-- 2020-01-29
