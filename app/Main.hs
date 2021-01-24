{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai ( responseLBS, Application )
import Network.HTTP.Types ( status200 )
import Network.Wai.Handler.Warp (run)

import qualified Effects

main' :: IO ()
main' = Effects.main

app :: Application
app _ respond = do
  putStrLn "I've done some IO here"
  respond $
    responseLBS
      status200
      [("Content-Type", "text/plain")]
      "Hello, Web!"

main :: IO ()
main = do
  putStrLn "http://localhost:8080/"
  run 8080 app