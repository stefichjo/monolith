{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds, RankNTypes #-}
module Effects.B_Domain where
import Effects.A_Model

import Polysemy
-- TODO default implementations

class Monad m => Console m where

  consoleRead :: m String
  consoleWrite :: String -> m ()

class Monad m => DB m where

  dbCreate :: User -> m ()
  dbRead :: m [User]

  dbNextUser :: UserName -> m User
  dbNextUser name = do
    User lastId _ <- maximum <$> dbRead
    return $ User (succ lastId) name

class Monad m => Log m where

  logWrite :: String -> m ()

data ConsoleSem m a where {

    ConsoleSemWrite :: String -> ConsoleSem m ();
    ConsoleSemRead :: ConsoleSem m String;

  }; makeSem ''ConsoleSem

data DbSem m a where {

    DbSemCreate :: User -> DbSem m ();
    DbSemRead :: DbSem m [User];

  }; makeSem ''DbSem

data LogSem m a where {

    LogSemWrite :: String -> LogSem m ();
  
  }; makeSem ''LogSem

type DbSemNextUser r a = Members '[DbSem] r => Sem r a
dbSemNextUser :: UserName -> DbSemNextUser r User
dbSemNextUser name = do
  User lastId  _ <- maximum <$> dbSemRead
  return $ User (succ lastId) name
