module Effects.Spec where

import Effects.B_Language.Spec ( specB_Language )
import Test.Hspec ( Spec )

specEffects :: Spec
specEffects = do

  specB_Language


