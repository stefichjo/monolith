{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
module Effects.B_Language where
import Effects.B_Domain
import Effects.A_Model

import Polysemy
import FileSystem
import Effects.Config

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

runSemIO :: Sem '[ConsoleSem, DbSem, LogSem, Embed IO] Event -> IO Event
runSemIO =
  runM
    .
      (interpret $ \case
        LogSemWrite msg -> embed $ addFile logFileName msg)
    .
      (interpret $ \case
        DbSemCreate user -> embed $ addFile dbFileName $ user
        DbSemRead        -> embed $ map read . lines <$> readFileContents dbFileName)
    . 
      (interpret $ \case
        ConsoleSemWrite line -> embed $ putStrLn line
        ConsoleSemRead       -> embed getLine)

-- type Builder r = Sem r
-- type With dsl r = forall a. Builder (dsl ': r) a -> Builder r a
-- type WithIO r = Member (Embed IO) r
-- type Build m a = Monad m => Builder '[Embed m] a -> m a
-- build :: Monad m => Sem '[Embed m] a -> m a
-- build = runM
