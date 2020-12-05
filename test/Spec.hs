{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Test.Hspec
import Test.QuickCheck

import Effects
import Utils

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

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

ok = and [
  okLog,
  okDB,
  okConsole,
  okApp,
  okLanguage,
  True]

okLog = and [

    -- LogT ()
    runLogT (logWrite "Hi!")
    ==
    ((), "Hi!"),
    
    -- LogMtl ()
    runLog (logWrite "Hi!")
    ==
    ((), "Hi!"),
    
  True] where

  runLogT = runWriter
  runLog = runLogT . runLogMtl

okDB = and [

  -- DBT ()
  runDBT (dbCreate (last inMemoryDB)) (init inMemoryDB)
  ==
  ((), inMemoryDB),
  
  -- DBT [User]
  runDBT dbRead inMemoryDB
  ==
  (inMemoryDB, inMemoryDB),

  -- DBMtl ()
  runDB (dbCreate (last inMemoryDB)) (init inMemoryDB)
  ==
  ((), inMemoryDB),
  
  -- DBMtl [User]
  runDB dbRead inMemoryDB
  ==
  (inMemoryDB, inMemoryDB),

  True] where

  runDBT = runState
  runDB = runDBT . runDBMtl

okConsole = and [

  -- ConsoleT String
  runConsoleT (consoleRead) "Hi!"
  ==
  ("Hi!", ""),
  
  -- ConsoleT ()
  runConsoleT (consoleWrite "Hi!") ""
  ==
  ((), "Hi!"),
  
  
  -- ConsoleMtl String
  runConsole (consoleRead) "Hi!"
  ==
  ("Hi!", ""),

  -- ConsoleMtl ()
  runConsole (consoleWrite "Hi!") ""
  ==
  ((), "Hi!"),
  
  True] where
  
  runConsoleT = runReader . runWriterT
  runConsole = runConsoleT . runConsoleMtl

okApp = and [

    -- AppMtl ()
    runApp app inMemoryDB "Fizz"
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

type LogT = Writer String
instance Log LogT where

  logWrite = tell
newtype LogMtl a = LogMtl { runLogMtl :: LogT a }
  deriving (Functor, Applicative, Monad, MonadWriter String)
instance Log LogMtl where

  logWrite msg = LogMtl $ logWrite msg
instance Log AppMtl where

  logWrite = tell

type DBT = State [User]
instance DB DBT where

  dbCreate user = dbRead >>= put . append user
  dbRead = get
newtype DBMtl a = DBMtl { runDBMtl :: DBT a }
  deriving (Functor, Applicative, Monad, MonadState [User])
instance DB DBMtl where

  dbCreate user = DBMtl $ dbCreate user
  dbRead = DBMtl $ get
instance DB AppMtl where

  dbCreate user = dbRead >>= put . append user
  dbRead = get

type ConsoleT = WriterT String (Reader String)
instance Console ConsoleT where

  consoleRead = ask
  consoleWrite = tell
instance Console ConsoleMtl where
  consoleRead = ConsoleMtl $ consoleRead
  consoleWrite msg = ConsoleMtl $ consoleWrite msg
newtype ConsoleMtl a =
  ConsoleMtl {
    runConsoleMtl :: ConsoleT a }
  deriving (Functor, Applicative, Monad, MonadReader String, MonadWriter String)
instance Console AppMtl where

  consoleRead = ask
  consoleWrite = tell

type AppT = StateT [User] ConsoleT
newtype AppMtl a =
  AppMtl {
    runAppMtl :: AppT a }
  deriving (Functor, Applicative, Monad, MonadReader String, MonadWriter String, MonadState [User])

inMemoryDB = [
    User 42 "Bar",
    User 23 "Foo"
  ]

{-
Tagles Final:
https://serokell.io/blog/tagless-final
https://jproyo.github.io/posts/2019-03-17-tagless-final-haskell.html

Free Monads:
https://www.youtube.com/watch?v=gUPuWHAt6SA&t=57s

Polysemy:
https://www.youtube.com/watch?v=eth4y015BCU

TODO
* DSL (GADT)
  * in Spec
* DIY Member/Effect Interpreter "T"
* Member, Eff
* Sem Polysemy Interpreter
-}

{-
withdraw :: (
  Member Bank r,
  Member Logger r)
  => Int
  -> Eff r (Maybe Int)
-}