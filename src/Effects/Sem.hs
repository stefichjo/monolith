{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
module Effects.Sem where
import Effects.Config
import Effects.A_Model
import FileSystem
import Polysemy

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

nextUser :: UserName -> App r User
nextUser name = do
  User lastId  _ <- maximum <$> dbSemRead
  return $ User (succ lastId) name

type App r a = Members '[ConsoleSem, DbSem, LogSem] r => Sem r a

app :: App r Event
app = do
  consoleSemWrite "Yes?"
  name <- consoleSemRead
  logSemWrite $ "New user: " <> name <> "."
  user <- nextUser name
  dbSemCreate user
  consoleSemWrite "Bye!"
  return user

type AppIO = IO

runIO :: Sem '[ConsoleSem, DbSem, LogSem, Embed AppIO] Event -> AppIO Event
runIO =
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
mainIO :: IO ()
mainIO = runIO app >>= print

-- type Builder r = Sem r
-- type With dsl r = forall a. Builder (dsl ': r) a -> Builder r a
-- type WithIO r = Member (Embed IO) r
-- type Build m a = Monad m => Builder '[Embed m] a -> m a
-- build :: Monad m => Sem '[Embed m] a -> m a
-- build = runM
