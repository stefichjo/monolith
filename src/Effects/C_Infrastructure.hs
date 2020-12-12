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

  dbRead = map read . lines <$> readFileContents dbFileName
  dbCreate = addFile dbFileName

instance Log IO where

  logWrite = addFile logFileName


interpretIO :: Sem '[ConsoleSem, DbSem, LogSem, Embed IO] a -> IO a
interpretIO =
  runM
    .
      (interpret $ \case
        LogSemWrite msg -> embed $ addFile logFileName msg)
    .
      (interpret $ \case
        DbSemRead        -> embed $ map read . lines <$> readFileContents dbFileName
        DbSemCreate user -> embed $ addFile dbFileName $ user)
    . 
      (interpret $ \case
        ConsoleSemRead       -> embed getLine
        ConsoleSemWrite line -> embed $ putStrLn line)

