module Effects.Fixtures where
import Effects.Utils

import Control.Monad.Identity

consoleMock = "Fizz" :: UserName
lastUserIdMock = 42 :: UserId

dbMock = [User {userId = lastUserIdMock, userName = "Bar"},User {userId = 23, userName = "Foo"}] :: [User]

type AppMock = Identity