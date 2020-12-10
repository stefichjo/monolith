module Effects.Spec where

import Test.Hspec
import Test.QuickCheck

import Effects.Mtl.Spec
import Effects.Sem.Spec

specEffects :: Spec
specEffects = do

  specMtl

  specSem