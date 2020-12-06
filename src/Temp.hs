{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Temp where

import Data.Function
import System.Random (randomIO)
import Polysemy

data Console m a where
  PrintLine :: String -> Console m ()
  ReadLine :: Console m String

data Random v m a where
  NextRandom :: Random v m v

makeSem ''Console
makeSem ''Random

withConsoleIO ::
     Member (Embed IO) r
  => Sem (Console ': r) a -> Sem r a
withConsoleIO = interpret $ \case
  PrintLine line -> embed (putStrLn line)
  ReadLine -> embed getLine

withRandomIO ::
     Member (Embed IO) r
  => Sem (Random Int ': r) a -> Sem r a
withRandomIO = interpret $ \case
  NextRandom -> embed randomIO

programBuilder ::
     Member Console r
  => Member (Random Int) r
  => Sem r Int
programBuilder = do
  printLine "Insert your number:"
  i1 <- readLine
  i2 <- nextRandom
  pure (read i1 + i2)

main' :: IO ()
main' = program >>= putStrLn . show
  where
    program = programBuilder
      & withConsoleIO
      & withRandomIO
      & build

build = runM
