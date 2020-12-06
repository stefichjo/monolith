module Lib
    ( someFunc
    ) where

import Polysemy
import Control.Monad.State
import Effects
import Temp

someFunc :: IO ()
someFunc = putStrLn "someFunc"
