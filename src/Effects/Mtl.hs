{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
module Effects.Mtl where
import Effects.Utils

import FileSystem
import Utils (append)

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

instance Console IO where

  consoleRead = getLine
  consoleWrite = putStrLn
instance DB IO where

  dbCreate = addFile dbFileName
  dbRead = map read . lines <$> readFileContents dbFileName
instance Log IO where

  logWrite = addFile logFileName

