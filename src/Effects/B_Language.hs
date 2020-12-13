{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Effects.B_Language where
import Effects.B_Domain
    ( consoleSemRead,
      consoleSemWrite,
      dbSemCreate,
      dbSemNextUser,
      logSemWrite,
      Console(..),
      ConsoleSem,
      DB(dbCreate, dbNextUser),
      DbSem,
      Log(..),
      LogSem )
import Effects.A_Model ( Event )

import Polysemy ( Members, Sem )

type App m a = (Console m, DB m, Log m) => m a
app :: App m Event
app = do
  consoleWrite "Yes?"
  name <- consoleRead
  logWrite $ "New user: " <> name <> "."
  user <- dbNextUser name
  dbCreate user
  consoleWrite "Bye!"
  return user

type AppSem r a = Members '[ConsoleSem, DbSem, LogSem] r => Sem r a
appSem :: AppSem r Event
appSem = do
  consoleSemWrite "Yes?"
  name <- consoleSemRead
  logSemWrite $ "New user: " <> name <> "."
  user <- dbSemNextUser name
  dbSemCreate user
  consoleSemWrite "Bye!"
  return user
