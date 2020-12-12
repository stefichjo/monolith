{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators, ConstraintKinds, GADTs #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}

module Effects.Utils where

import Effects.A_Model
import Effects.B_Domain

-- TODO Config.hs ("Stage.hs" ?)

logFileName = "log" :: FilePath
dbFileName = "db" :: FilePath

-- type Builder r = Sem r
-- type With dsl r = forall a. Builder (dsl ': r) a -> Builder r a
-- type WithIO r = Member (Embed IO) r
-- type Build m a = Monad m => Builder '[Embed m] a -> m a
-- build :: Monad m => Sem '[Embed m] a -> m a
-- build = runM

-- mainIO :: IO ()
-- mainIO = app >>= print

-- TODO apparently, these aren't utils but the architecture itself.