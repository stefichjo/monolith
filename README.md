# monolith

* `stack new monolith`
* `cd monolith`

* `vim package.yaml`
``` .yaml
library:
  source-dirs: src
  dependencies:
    - mtl
    - polysemy
```

* `vim app/Main.hs`
``` .hs
import Lib
import Polysemy
```

* `stack build --file-watch --fast`

#

{-
Tagles Final:
https://serokell.io/blog/tagless-final
https://jproyo.github.io/posts/2019-03-17-tagless-final-haskell.html

Polysemy:
https://hackage.haskell.org/package/polysemy

Clean Architecture:
https://github.com/thma/PolysemyCleanArchitecture

Events:
https://blog.jayway.com/2013/06/20/dont-publish-domain-events-return-them/
-}

-- REFACTOR JSON

-- TODO use DBT and LogT as well
