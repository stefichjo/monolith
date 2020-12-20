{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Effects.B_Language where
import Effects.B_Domain
    ( consoleSemRead,
      dbSemCreate,
      dbSemNextUser,
      logSemWrite,
      Console(..),
      ConsoleSem,
      DB(dbCreate, dbNextUser),
      DbSem,
      Log(..),
      LogSem )
import Effects.A_Model ( Event, User, UserId, UserName )

import Polysemy ( Members, Sem )

type App m a = (Console m, DB m, Log m) => m a
app :: App m Event
app = do
  name <- consoleRead
  logWrite $ "New user: " <> name <> "."
  user <- dbNextUser name
  dbCreate user
  return user

type AppSem r a = Members '[ConsoleSem, DbSem, LogSem] r => Sem r a
appSem :: AppSem r Event
appSem = do
  name <- consoleSemRead
  logSemWrite $ "New user: " <> name <> "."
  user <- dbSemNextUser name
  dbSemCreate user
  return user

-- TODO acknowledge Event
-- TODO execute Command
-- TODO evaluate Query

-- TODO UserName as argument of app or app runner (execute command)


newtype Query = ReadUser UserId

newtype Command = CreateNewUser UserName

execute :: Command -> App m a
execute (CreateNewUser userName) = undefined -- runReader app userName

newtype Event' = NewUserCreated User

acknowledge :: Event' -> App m a
acknowledge (NewUserCreated user) = undefined 
