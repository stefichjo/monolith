module Effects.Fixtures where

import Effects.Utils

consoleConst = "10" :: UserName

inMemoryDB = [User {userId = 42, userName = "Bar"},User {userId = 23, userName = "Foo"}] :: [User]
