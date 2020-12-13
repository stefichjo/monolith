{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds, RankNTypes #-}
module Effects.B_Domain where
import Effects.A_Model ( User(User), UserName )

import Polysemy ( Members, Sem, makeSem )

class Monad m => Console m where

  consoleRead :: m String
  consoleWrite :: String -> m ()

class Monad m => DB m where

  dbRead :: m [User]
  dbCreate :: User -> m ()

  dbNextUser :: UserName -> m User
  dbNextUser name = do
    User lastId _ <- maximum <$> dbRead
    return $ User (succ lastId) name

class Monad m => Log m where

  logWrite :: String -> m ()

data ConsoleSem m a where {

    ConsoleSemRead :: ConsoleSem m String;
    ConsoleSemWrite :: String -> ConsoleSem m ();

  }; makeSem ''ConsoleSem

data DbSem m a where {

    DbSemRead :: DbSem m [User];
    DbSemCreate :: User -> DbSem m ();

  }; makeSem ''DbSem

data LogSem m a where {

    LogSemWrite :: String -> LogSem m ();
  
  }; makeSem ''LogSem

type DbSemNextUser r a = Members '[DbSem] r => Sem r a
dbSemNextUser :: UserName -> DbSemNextUser r User
dbSemNextUser name = do
  User lastId  _ <- maximum <$> dbSemRead
  return $ User (succ lastId) name
