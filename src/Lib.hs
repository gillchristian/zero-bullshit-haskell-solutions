module Lib where

import qualified Zero.Server as Server

helloHandler :: Server.Request -> Server.Response
helloHandler _
  = Server.stringResponse "hello"

echoHandler :: Server.Request -> Server.Response
echoHandler = Server.stringResponse . Server.requestBody

run :: IO ()
run
  = Server.startServer
      [ Server.simpleHandler Server.GET "/hello" helloHandler
      , Server.simpleHandler Server.POST "/echo" echoHandler
      ]
