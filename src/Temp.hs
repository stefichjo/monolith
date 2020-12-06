{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Temp where

import Polysemy
import Data.Function ((&))
import System.Random (randomIO)

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
  ReadLine       -> embed getLine

withRandomIO ::
     Member (Embed IO) r
  => Sem (Random Int ': r) a -> Sem r a
withRandomIO = interpret $ \case
  NextRandom -> embed randomIO

runConsoleConst :: String -> Sem (Console ': r) a -> Sem r a
runConsoleConst constLine = interpret $ \case
  PrintLine line -> pure ()
  ReadLine -> pure constLine

runRandomConst :: Int -> Sem (Random Int ': r) a -> Sem r a
runRandomConst v = interpret $ \case
  NextRandom -> pure v

type ProgramBuilder r a =
     Member Console r
  => Member (Random a) r
  => Sem r a

programBuilder :: ProgramBuilder r Int
programBuilder = do
  printLine "Insert your number:"
  i1 <- readLine
  i2 <- nextRandom
  pure (read i1 + i2)

main' :: IO ()
main' = programM >>= putStrLn . show

programM :: IO Int
programM = programBuilder
  & withConsoleIO
  & withRandomIO
  & runM

program :: Int
program = programBuilder
  & runConsoleConst "10"
  & runRandomConst 20
  & run

-- generalize: `withConsole`, `withRandom`, `build`

-- IDE
-- jump to definition
-- Hoogle