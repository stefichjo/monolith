{-# LANGUAGE ConstrainedClassMethods #-}
module Effects.B_Domain where
import Effects.A_Model

-- TODO default implementations

class Monad m => Console m where

  consoleRead :: m String
  consoleRead = return "..."
  consoleWrite :: String -> m ()
  consoleWrite msg = return ()
class Monad m => DB m where

  dbCreate :: User -> m ()
  dbRead :: m [User]

  nextUser :: Monad m => UserName -> m User
  nextUser name = do
    User lastId _ <- maximum <$> dbRead
    return $ User (succ lastId) name
class Monad m => Log m where

  logWrite :: String -> m ()

