module Effects.Mtl.Spec where

import Test.Hspec
import Test.QuickCheck

import Effects
import Utils

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.Function

inMemoryDB = read inMemoryDbRaw

specMtl :: Spec
specMtl = do

  describe "test" $ do
    it "should work" $ do
      True `shouldBe` True

  describe "ok" $ do
    it "should be ok" $ do
      ok `shouldBe` True

okApp = and [

    -- AppMtl ()
    runApp app' inMemoryDB "sts"
    ==
    (,)
      (User {userId = 42, userName = "sts"}, (inMemoryDB <> [User 42 "sts"]))
      "Yes?New user: Fizz.Bye!",
      
  True]

runApp app db = runReader (runWriterT (runStateT (runAppMtl app) db))


ok = and [
  okLog,
  okDB,
  okConsole,
  -- okApp,
  True]
okLog = and [

    -- LogT ()
    runLogT (logWrite' "Hi!")
    ==
    ((), "Hi!"),
    
    -- LogMtl ()
    runLog (logWrite' "Hi!")
    ==
    ((), "Hi!"),
    
  True] where

  runLogT = runWriter
  runLog = runLogT . runLogMtl
okDB = and [

  -- DBT ()
  runDBT (dbCreate' (last inMemoryDB)) (init inMemoryDB)
  ==
  ((), inMemoryDB),
  
  -- DBT [User]
  runDBT dbRead' inMemoryDB
  ==
  (inMemoryDB, inMemoryDB),

  -- DBMtl ()
  runDB (dbCreate' (last inMemoryDB)) (init inMemoryDB)
  ==
  ((), inMemoryDB),
  
  -- DBMtl [User]
  runDB dbRead' inMemoryDB
  ==
  (inMemoryDB, inMemoryDB),

  True] where

  runDBT = runState
  runDB = runDBT . runDBMtl
okConsole = and [

  -- ConsoleT String
  runConsoleT (consoleRead') "Hi!"
  ==
  ("Hi!", ""),
  
  -- ConsoleT ()
  runConsoleT (consoleWrite' "Hi!") ""
  ==
  ((), "Hi!"),
  
  
  -- ConsoleMtl String
  runConsole (consoleRead') "Hi!"
  ==
  ("Hi!", ""),

  -- ConsoleMtl ()
  runConsole (consoleWrite' "Hi!") ""
  ==
  ((), "Hi!"),
  
  True] where
  
  runConsoleT = runReader . runWriterT
  runConsole = runConsoleT . runConsoleMtl
