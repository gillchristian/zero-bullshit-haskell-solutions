module Lib where

import qualified Zero.Server as Server

helloHandler :: Server.Request -> Server.Response
helloHandler _
  = Server.stringResponse "hello"

-- Change `someFunc` to `run` because we're not savages.
-- You'll have to update this in app/Main.hs as well.
run :: IO ()
run
  = Server.startServer
      [ Server.simpleHandler Server.GET "/hello" helloHandler
      ]
