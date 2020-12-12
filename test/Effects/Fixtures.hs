module Effects.Fixtures where
import Effects.A_Model

import Control.Monad.Identity

consoleMock = "Fizz" :: UserName
lastUserIdMock = 42 :: UserId

dbMock = [User {userId = lastUserIdMock, userName = "Bar"},User {userId = 23, userName = "Foo"}] :: [User]

type AppMock = Identity

expectedUser = User {userId = succ lastUserIdMock, userName = consoleMock} :: User
