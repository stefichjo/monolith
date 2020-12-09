{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators, ConstraintKinds, GADTs #-}

module Utils where

import Polysemy

append :: a -> [a] -> [a]
append = (flip (<>)) . pure


-- TODO Config.hs

logFileName = "log" :: FilePath
dbFileName = "db" :: FilePath

consoleConst = "10" :: String
randomConst = 20 :: Int

inMemoryDbRaw = "[User {userId = 42, userName = \"Bar\"},User {userId = 23, userName = \"Foo\"}]"

-- type Builder r = Sem r
-- type With dsl r = forall a. Builder (dsl ': r) a -> Builder r a
-- type WithIO r = Member (Embed IO) r
-- type Build m a = Monad m => Builder '[Embed m] a -> m a
build :: Monad m => Sem '[Embed m] a -> m a
build = runM
