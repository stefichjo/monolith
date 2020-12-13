{-# LANGUAGE DeriveGeneric #-}
module Reservations where

import Data.Time.Calendar ( Day )
import Numeric.Natural ( Natural )
import GHC.Generics ( Generic )
import Data.Map (fromList,  Map )
import Data.Time.Format (parseTimeOrError, readTime, defaultTimeLocale)

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

-- >>> parseDay "2010-10-11"
-- 2010-10-11

parseDay :: String -> Day
parseDay = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

foo :: ReservationMap
foo = fromList
  [
    (
      parseDay "2020-06-01",
        [
          Reservation {date = parseDay "2020-06-01", name = "Mr. Miller", email = "manfred@miller.com", quantity = 2}, 
          Reservation {date = parseDay "2020-06-01", name = "Andrew M. Jones", email = "amjones@example.com", quantity = 4}
        ]
    )
  ]

