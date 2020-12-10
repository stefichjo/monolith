module Effects.Fixtures where
import Effects.Utils

import Control.Monad.Identity

consoleMock = "10" :: UserName

dbMock = [User {userId = 42, userName = "Bar"},User {userId = 23, userName = "Foo"}] :: [User]

type AppMock = Identity