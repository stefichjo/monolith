# monolith

* `stack new monolith`
* `cd monolith`

* `vim monolith.cabal`
``` .cabal
executable monolith-exe
  main-is: Main.hs
  other-modules:
      Paths_monolith
  hs-source-dirs:
      app
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      base >=4.7 && <5
    , monolith
    , polysemy
```

* `vim app/Main.hs`
``` .hs
import Lib
import Polysemy
```

* `stack build --file-watch --fast`
