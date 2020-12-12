module Effects.Mtl where
import Effects.Utils

import FileSystem
import Effects.B_Domain 

instance Console IO where

  consoleRead = getLine
  consoleWrite = putStrLn
instance DB IO where

  dbCreate = addFile dbFileName
  dbRead = map read . lines <$> readFileContents dbFileName
instance Log IO where

  logWrite = addFile logFileName

