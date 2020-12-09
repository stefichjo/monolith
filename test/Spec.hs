{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Test.Hspec
import Test.QuickCheck

import Effects
import Utils

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.Function

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "ok" $ do
    it "should be ok" $ do
      ok `shouldBe` True

  describe "append" $ do
    it "should append" $ property $
      \x -> append x "Hi" `shouldBe` "Hi" <> [x]      

  describe "app builder" $ do
    it "can build an app that adds two numbers together (random and from console)" $ do
      appMock `shouldBe` Identity ()

ok = and [
  okLog,
  okDB,
  okConsole,
  okApp,
  okLanguage,
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
okApp = and [

    -- AppMtl ()
    runApp app' inMemoryDB "Fizz"
    ==
    (,)
      ((), (inMemoryDB <> [User 43 "Fizz"]))
      "Yes?New user: Fizz.Bye!",
      
  True] where

  runApp app db = runReader (runWriterT (runStateT (runAppMtl app) db))
okLanguage = and [

    -- UserId
    userId (User 42 "Hello World!")
    ==
    42,

    -- UserName
    userName (User 42 "Hello World!")
    ==
    "Hello World!",

  True]

inMemoryDB = read inMemoryDbRaw

{-
Tagles Final:
https://serokell.io/blog/tagless-final
https://jproyo.github.io/posts/2019-03-17-tagless-final-haskell.html

Polysemy:
https://hackage.haskell.org/package/polysemy

Clean Architecture:
https://github.com/thma/PolysemyCleanArchitecture

Events:
https://blog.jayway.com/2013/06/20/dont-publish-domain-events-return-them/
-}

-- TODO Event instead of ()
-- REFACTOR JSON

-- TODO move to submodules Log'/Log
-- TODO use DBT and LogT as well

