module Lib
    ( someFunc
    ) where

import Polysemy
import Control.Monad.State

someFunc :: IO ()
someFunc = putStrLn "someFunc"
