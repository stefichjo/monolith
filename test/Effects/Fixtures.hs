module Effects.Fixtures where
import Effects.A_Model

consoleMock = "Fizz" :: UserName
lastUserIdMock = 42 :: UserId

dbMock = [User {userId = lastUserIdMock, userName = "Bar"},User {userId = 23, userName = "Foo"}] :: [User]

expectedUser = User {userId = succ lastUserIdMock, userName = consoleMock} :: User
