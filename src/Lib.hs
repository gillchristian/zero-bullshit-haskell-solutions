module Lib where

import qualified Zero.Server as Server

helloHandler :: Server.Request -> Server.Response
helloHandler _ =
  Server.stringResponse "hello"

echoHandler :: Server.Request -> Server.Response
echoHandler = Server.stringResponse . Server.requestBody

caseHandler :: Server.Request -> Server.Response
caseHandler req =
  Server.stringResponse $ case Server.requestBody req of
    "1" -> "one"
    "2" -> "two"
    "3" -> "three"
    _ -> "What am I, a mathematician?"

run :: IO ()
run =
  Server.startServer
    [ Server.simpleHandler Server.GET "/hello" helloHandler,
      Server.simpleHandler Server.POST "/echo" echoHandler,
      Server.simpleHandler Server.POST "/case" caseHandler
    ]
