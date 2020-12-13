{-# LANGUAGE RankNTypes, ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Effects.B_Language.Mtl.Spec where
import Effects.B_Language ( app )
import Effects.B_Domain
    ( Console(..), DB(dbRead, dbCreate), Log(..) )
import Effects.A_Model ( User )
import Effects.Fixtures ( consoleMock, dbMock, expectedUser )

import Test.Hspec ( Spec, describe, it, shouldBe )

import Utils ( append )

import Control.Monad.Reader ( MonadReader(ask), runReader, Reader )
import Control.Monad.Writer
    ( WriterT(runWriterT), MonadWriter(tell) )
import Control.Monad.State ( StateT(..), MonadState(put, get) )

specMtl :: Spec
specMtl = do

  describe "app (mtl)" $ do
    it "ok" $ do
      runApp app dbMock consoleMock
      `shouldBe`
      (,)
        (expectedUser, dbMock <> [expectedUser])
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

