{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Effects.B_Language.Mtl.Spec where
import Effects.B_Language
import Effects.B_Domain
import Effects.A_Model
import Effects.Fixtures

import Test.Hspec

import Utils

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

specMtl :: Spec
specMtl = do

  describe "app (mtl)" $ do
    it "ok" $ do
      runApp app dbMock consoleMock
      `shouldBe`
      (,)
        (expectedUser, (dbMock <> [expectedUser]))
        "Yes?New user: Fizz.Bye!"

  where

    runApp app = runReader . runWriterT . runStateT (runAppMtl app)

type AppT = StateT [User] (WriterT String (Reader String))
newtype AppMtl a =

  AppMtl {
    runAppMtl :: AppT a }
  deriving (
    Functor, Applicative, Monad,
    MonadReader String, MonadWriter String, MonadState [User])

instance Console AppMtl where

  consoleRead = ask
  consoleWrite = tell

instance DB AppMtl where

  dbRead = get
  dbCreate user = dbRead >>= put . append user

instance Log AppMtl where

  logWrite = tell

