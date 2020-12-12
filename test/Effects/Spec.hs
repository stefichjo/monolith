module Effects.Spec where

import Effects.B_Language.Spec
import Effects.B_Language.Mtl.Spec
import Effects.B_Language.Sem.Spec

import Test.Hspec

import Utils

specEffects :: Spec
specEffects = do

  specB_Language

  specMtl

  specSem

