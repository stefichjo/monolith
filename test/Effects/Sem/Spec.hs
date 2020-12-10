{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
module Effects.Sem.Spec where
import Effects.Sem
import Effects.Utils

import Test.Hspec
import Test.QuickCheck

import Utils
import Polysemy

specSem :: Spec
specSem = do

  describe "app" $ do
    it "can" $ do
      appMock `shouldBe` return (User {userId = 43, userName = "10"})

runMock :: Sem '[Console, DB, Log, Embed AppMock] Event -> AppMock Event
runMock =
  runM
    .
      (interpret $ \case
        LogWrite msg -> return ())
    .
      (interpret $ \case
        DbCreate user -> return ()
        DbRead        -> return $ read inMemoryDbRaw)
    .
      (interpret $ \case
        ConsoleWrite line -> return ()
        ConsoleRead       -> return consoleConst)
appMock :: AppMock Event
appMock = runMock app
mainMock :: IO ()
mainMock = print appMock

-- TODO mtl-like app instance