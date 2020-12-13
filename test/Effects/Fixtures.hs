module Effects.Fixtures where
import Effects.A_Model

consoleMock :: UserName
consoleMock = "Fizz"

lastUserIdMock :: UserId
lastUserIdMock = 42

dbMock :: [User]
dbMock = [User {userId = lastUserIdMock, userName = "Bar"},User {userId = 23, userName = "Foo"}]

expectedUser :: User
expectedUser = User {userId = succ lastUserIdMock, userName = consoleMock}
