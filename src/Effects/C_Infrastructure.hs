{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, GADTs #-}
module Effects.C_Infrastructure where
import Effects.B_Domain
import Effects.Config

import FileSystem
import Polysemy

instance Console IO where

  consoleRead = getLine
  consoleWrite = putStrLn

instance DB IO where

  dbCreate = addFile dbFileName
  dbRead = map read . lines <$> readFileContents dbFileName

instance Log IO where

  logWrite = addFile logFileName


runSemIO :: Sem '[ConsoleSem, DbSem, LogSem, Embed IO] a -> IO a
runSemIO =
  runM
    .
      (interpret $ \case
        LogSemWrite msg -> embed $ addFile logFileName msg)
    .
      (interpret $ \case
        DbSemCreate user -> embed $ addFile dbFileName $ user
        DbSemRead        -> embed $ map read . lines <$> readFileContents dbFileName)
    . 
      (interpret $ \case
        ConsoleSemWrite line -> embed $ putStrLn line
        ConsoleSemRead       -> embed getLine)

