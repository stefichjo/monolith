{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, GADTs #-}
module Effects.C_Infrastructure where
import Effects.B_Domain
    ( Console(..),
      ConsoleSem(..),
      DB(dbRead, dbCreate),
      DbSem(..),
      Log(..),
      LogSem(..) )
import Effects.Config ( dbFileName, logFileName )

import FileSystem ( addFile, readFileContents )
import Polysemy ( Sem, embed, runM, interpret, Embed )

instance Console IO where

  consoleRead = getLine

instance DB IO where

  dbRead = map read . lines <$> readFileContents dbFileName
  dbCreate = addFile dbFileName

instance Log IO where

  logWrite = addFile logFileName

interpretIO :: Sem '[ConsoleSem, DbSem, LogSem, Embed IO] a -> IO a
interpretIO =
  runM
    .
      interpret (\case
        LogSemWrite msg -> embed $ addFile logFileName msg)
    .
      interpret (\case
        DbSemRead        -> embed $ map read . lines <$> readFileContents dbFileName
        DbSemCreate user -> embed $ addFile dbFileName user)
    . 
      interpret (\case
        ConsoleSemRead -> embed getLine)
