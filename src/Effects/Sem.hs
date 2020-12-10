{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
module Effects.Sem where

import Utils
import Effects.Utils
import FileSystem
import Polysemy
import Data.Function ((&))

data Log m a where {

    LogWrite :: String -> Log m ();
  
  }; makeSem ''Log
data Console m a where {

    ConsoleWrite :: String -> Console m ();
    ConsoleRead :: Console m String;

  }; makeSem ''Console
data DB m a where {

    DbCreate :: User -> DB m ();
    DbRead :: DB m [User];

  }; makeSem ''DB

nextUser :: UserName -> App r User
nextUser name = do
  User lastId  _ <- maximum <$> dbRead
  return $ User (succ lastId) name

type App r a = Members '[Console, DB, Log] r => Sem r a

app :: Members '[Console, DB, Log] r => Sem r Event
app = do
  consoleWrite "Yes?"
  name <- consoleRead
  logWrite $ "New user: " <> name <> "."
  user <- nextUser name
  dbCreate user
  consoleWrite "Bye!"
  return user

type AppIO = IO

runIO :: Sem '[DB, Console, Log, Embed AppIO] Event -> AppIO Event
runIO =
  runM
    .
      (interpret $ \case
        LogWrite msg -> embed $ addFile logFileName msg)
    . 
      (interpret $ \case
        ConsoleWrite line -> embed $ putStrLn line
        ConsoleRead       -> embed getLine)
    .
      (interpret $ \case
        DbCreate user -> embed $ addFile dbFileName $ user
        DbRead        -> embed $ map read . lines <$> readFileContents dbFileName)
mainIO :: IO ()
mainIO = runIO app >>= print

-- runMock :: Sem '[DB, Console, Log, Embed AppMock] Event -> AppMock Event
runMock =
  runM
    .
      (interpret $ \case
        LogWrite msg -> return ())
    .
      (interpret $ \case
        ConsoleWrite line -> return ()
        ConsoleRead       -> return consoleConst)
    .
      (interpret $ \case
        DbCreate user -> return ()
        DbRead        -> return $ read inMemoryDbRaw)
appMock :: AppMock Event
appMock = runMock app
mainMock :: IO ()
mainMock = appMock & print