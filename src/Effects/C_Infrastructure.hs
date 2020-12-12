module Effects.C_Infrastructure where
import Effects.B_Domain

import Effects.Utils

import FileSystem

instance Console IO where

  consoleRead = getLine
  consoleWrite = putStrLn

instance DB IO where

  dbCreate = addFile dbFileName
  dbRead = map read . lines <$> readFileContents dbFileName

instance Log IO where

  logWrite = addFile logFileName
