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

type App' m a = (Console m, DB m, Log m) => m a
app' :: App' m Event
app' = do
  name <- consoleRead
  logWrite $ "New user: " <> name <> "."
  user <- dbNextUser name
  dbCreate user
  return user

type App m = (DB m, Log m) => Command -> m Event
app :: App m
app name = do
  logWrite $ "New user: " <> name <> "."
  user <- dbNextUser name
  dbCreate user
  return user

type AppFoo m = forall a. (DB m, Log m) => m a

type Run m = (DB m, Log m) => Command -> m [Event]
type Ack m = (DB m, Log m) => Event -> m ()
type Pub m = (DB m, Log m) => Event -> m [Command]

-- TODO event-sourcing: Run with DbRead (of foldL [Event])
-- TODO event-sourcing: Ack with DbWrite (of Event)
-- TODO Pub with PubSub (how?)

-- Far-fetchedly: instead of acknowledging the event, it is published to and subscribed by this app.
-- A peristence-command would, well, persist the event (i.e the payload of the command).
-- But it is also nice to have a guarantee that this app is first to update, before the next request is handled.



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


type Command = String