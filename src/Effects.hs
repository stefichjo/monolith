{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Effects (
  module Effects.Mtl,
  module Effects.Sem,
  module Effects
) where

import FileSystem
import Utils
import System.Random (randomIO)
import qualified Data.IntMap

import Effects.Mtl
import Effects.Sem
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity



type LogT = Writer String
instance Log' LogT where

  logWrite' = tell
newtype LogMtl a =
  
  LogMtl {
    runLogMtl :: LogT a }
  deriving (
    Functor, Applicative, Monad,
    MonadWriter String)
instance Log' LogMtl where

  logWrite' msg = LogMtl $ logWrite' msg

type ConsoleT = WriterT String (Reader String)
instance Console' ConsoleT where

  consoleRead' = ask
  consoleWrite' = tell
newtype ConsoleMtl a =

  ConsoleMtl {
    runConsoleMtl :: ConsoleT a }
  deriving (
    Functor, Applicative, Monad,
    MonadReader String, MonadWriter String)
instance Console' ConsoleMtl where

  consoleRead' = ConsoleMtl $ consoleRead'
  consoleWrite' msg = ConsoleMtl $ consoleWrite' msg

type DBT = State [User]
instance DB' DBT where

  dbCreate' user = dbRead' >>= put . append user
  dbRead' = get
newtype DBMtl a =
  
  DBMtl {
    runDBMtl :: DBT a }
  deriving (
    Functor, Applicative, Monad,
    MonadState [User])
instance DB' DBMtl where

  dbCreate' user = DBMtl $ dbCreate' user
  dbRead' = DBMtl $ get

type AppT = StateT [User] ConsoleT
newtype AppMtl a =

  AppMtl {
    runAppMtl :: AppT a }
  deriving (
    Functor, Applicative, Monad,
    MonadReader String, MonadWriter String, MonadState [User])
instance Log' AppMtl where

  logWrite' = tell
instance DB' AppMtl where

  dbCreate' user = dbRead' >>= put . append user
  dbRead' = get
instance Console' AppMtl where

  consoleRead' = ask
  consoleWrite' = tell
