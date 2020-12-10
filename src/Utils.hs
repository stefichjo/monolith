module Utils where

append :: a -> [a] -> [a]
append = (flip (<>)) . pure


