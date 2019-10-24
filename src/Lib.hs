module Lib where

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified Zero.Server as Server

-- Exercise 01

helloHandler :: Server.Request -> Server.Response
helloHandler _ =
  Server.stringResponse "hello"

-- Exercise 02

echoHandler :: Server.Request -> Server.Response
echoHandler = Server.stringResponse . Server.requestBody

-- Exercise 03

caseHandler :: Server.Request -> Server.Response
caseHandler req =
  Server.stringResponse $ case Server.requestBody req of
    "1" -> "one"
    "2" -> "two"
    "3" -> "three"
    _ -> "What am I, a mathematician?"

-- Exercise 04

stringManipulationHandler :: Server.Request -> Server.Response
stringManipulationHandler req =
  Server.stringResponse $ List.intercalate " " $
    case words $ Server.requestBody req of
      ("I'm" : "positive" : s) -> ("I" : "think" : s)
      s -> s

-- Exercise 05

data State = On | Off deriving (Eq, Show)

onOffSwitchHandler :: State -> Server.Request -> (State, Server.Response)
onOffSwitchHandler On _ = (Off, Server.stringResponse $ show On)
onOffSwitchHandler Off _ = (On, Server.stringResponse $ show Off)

-- Exercise 06

currentCountHandler :: Int -> Server.Request -> (Int, Server.Response)
currentCountHandler n _ = (n, Server.stringResponse $ show n)

increaseHandler :: Int -> Server.Request -> (Int, Server.Response)
increaseHandler n _ = (n + 1, Server.stringResponse "")

-- Exercise 07

data Item
  = Item
      { model :: String,
        quantity :: Int
      }
  deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

type Items = Map.Map String Item

getCartHandler :: Items -> Server.Request -> (Items, Server.Response)
getCartHandler is _ =
  ( is,
    Server.jsonResponse . reverse . List.sortOn quantity $ Map.elems is
  )

postCartHandler :: Items -> Server.Request -> (Items, Server.Response)
postCartHandler items req =
  case Server.decodeJson $ Server.requestBody req of
    Right queryItem -> case Map.lookup (model queryItem) items of
      Nothing ->
        ( Map.insert (model queryItem) queryItem items,
          Server.stringResponse "ok"
        )
      Just prev ->
        ( Map.insert
            (model prev)
            (prev {quantity = (quantity queryItem) + (quantity prev)})
            items,
          Server.stringResponse "ok"
        )
    Left err -> (items, Server.failureResponse $ show err)

-- Server

run :: IO ()
run =
  Server.startServer
    [ Server.simpleHandler Server.GET "/hello" helloHandler,
      Server.simpleHandler Server.POST "/echo" echoHandler,
      Server.simpleHandler Server.POST "/case" caseHandler,
      Server.simpleHandler Server.POST "/string-manipulation" stringManipulationHandler,
      Server.handlersWithState
        On
        [Server.statefulHandler Server.POST "/onoff-switch" onOffSwitchHandler],
      Server.handlersWithState
        0
        [ Server.statefulHandler Server.GET "/current-count" currentCountHandler,
          Server.statefulHandler Server.POST "/increase" increaseHandler
        ],
      Server.handlersWithState
        Map.empty
        [ Server.statefulHandler Server.GET "/cart" getCartHandler,
          Server.statefulHandler Server.POST "/cart" postCartHandler
        ]
    ]
