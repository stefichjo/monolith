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

# Set Up Visual Studio Code

* https://gitlab.haskell.org/zander/haskell_set-up/-/wikis/linux_fancy

## ghcid

* `stack install ghcid`

## ghcide

* `cd ~/Downloads`
* `git clone https://github.com/digital-asset/ghcide.git`
* `cd ghcide`
* ~~`mv stack88.yaml stack.yaml`~~
  * there is no `stack88.yaml` :/
* edit `stack.yaml`
  * https://www.stackage.org/lts
    * copy current resolver, e.g. `lts-16.25`
    * paste current resolver under `resolver`
* `stack install`

## Visual Studio Code

* https://github.com/haskell/ghcide#using-with-vs-code
  * `Ctrl-P` + `ext install haskell.haskell`
* edit `stack.yaml`
  * update `resolver`, e.g. `url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/16/25.yaml`


# Literature

Tagles Final:
* https://serokell.io/blog/tagless-final
* https://jproyo.github.io/posts/2019-03-17-tagless-final-haskell.html

Polysemy:
* https://hackage.haskell.org/package/polysemy

Clean Architecture:
* https://github.com/thma/PolysemyCleanArchitecture

Events:
* https://blog.jayway.com/2013/06/20/dont-publish-domain-events-return-them/

(TODO)
* https://haskell-explained.gitlab.io/blog/posts/2019/07/28/polysemy-is-cool-part-1/
* https://haskell-explained.gitlab.io/blog/posts/2019/07/31/polysemy-is-cool-part-2/