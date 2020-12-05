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
