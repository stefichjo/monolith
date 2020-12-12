{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, ConstrainedClassMethods #-}
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, DataKinds, PolyKinds #-}
module Effects.Sem.Spec where
import Effects.Fixtures
import Effects.A_Model
import Effects.B_Domain
import Effects.B_Language

import Test.Hspec

import Utils
import Polysemy

-- TODO mtl-like app instance
-- TODO run with state, writer and reader