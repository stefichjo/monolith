-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Effects.B_Domain where
import Effects.A_Model

import Polysemy
-- TODO default implementations

class Monad m => Console m where

  consoleRead :: m String
  consoleRead = return "..."

  consoleWrite :: String -> m ()
  consoleWrite msg = return ()

class Monad m => DB m where

  dbCreate :: User -> m ()
  dbRead :: m [User]

  dbNextUser :: Monad m => UserName -> m User
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

