{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators, ConstraintKinds, GADTs #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}

module Effects.Utils where

-- TODO Config.hs ("Stage.hs" ?)

logFileName = "log" :: FilePath
dbFileName = "db" :: FilePath

-- type Builder r = Sem r
-- type With dsl r = forall a. Builder (dsl ': r) a -> Builder r a
-- type WithIO r = Member (Embed IO) r
-- type Build m a = Monad m => Builder '[Embed m] a -> m a
-- build :: Monad m => Sem '[Embed m] a -> m a
-- build = runM

-- APPLICATION = DSL = LANGUAGE

type App m a = (Console m, DB m, Log m) => m a
app :: App m Event
app = do
  consoleWrite "Yes?"
  name <- consoleRead
  logWrite $ "New user: " <> name <> "."
  user <- nextUser name
  dbCreate user
  consoleWrite "Bye!"
  return user

-- mainIO :: IO ()
-- mainIO = app >>= print

-- MODEL

type UserId = Int
type UserName = String
data User =
  
  User {
    userId :: UserId,
    userName :: UserName
  }
  deriving (
    Eq, Ord, Show, Read)

type Event = User

-- DOMAIN

class Monad m => Console m where

  consoleRead :: m String
  consoleRead = return "..."
  consoleWrite :: String -> m ()
  consoleWrite msg = return ()
class Monad m => DB m where

  dbCreate :: User -> m ()
  dbRead :: m [User]

  nextUser :: Monad m => UserName -> m User
  nextUser name = do
    User lastId _ <- maximum <$> dbRead
    return $ User (succ lastId) name
class Monad m => Log m where

  logWrite :: String -> m ()

-- TODO apparently, these aren't utils but the architecture itself.