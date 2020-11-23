{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Account = Account
  { accountId        :: Int
  , accountFirstName :: String
  , accountLastName  :: String
  , accountLogin :: String
  , accountPassword  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Account)

type API = "getAccounts" :> Get '[JSON] [Account]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return getAccounts

getAccounts :: [Account]
getAccounts = [ Account 1 "Isaac" "Newton" "isya" "qwerty"
           , Account 2 "Albert" "Einstein" "alb" "asdfgh"
           ]
