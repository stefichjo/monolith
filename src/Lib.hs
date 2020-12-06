module Lib
    ( someFunc
    ) where

import Polysemy
import Control.Monad.State
import Effects

someFunc :: IO ()
someFunc = putStrLn "someFunc"
