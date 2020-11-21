{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module ApiType
    ( startApp
    , app
    ) where

import Data.Text
import Data.Time (UTCTime)
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API

$(deriveJSON defaultOptions ''User)

type UserAPI = "users" :> QueryParam "sortBy" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registration_date :: UTCTime
}

type RootEndpoint =
  Get '[JSON] User

type UserAPI2 = "users" :> "list-all" :> Get '[JSON] [User]
           :<|> "list-all" :> "users" :> Get '[JSON] [User]

type UserAPI3 = "users" :> "list-all" :> "now" :> Get '[JSON] [User]
              -- describes an endpoint reachable at:
              -- /users/list-all/now

type UserAPI4 = "users" :> Get '[JSON] [User]
           :<|> "admins" :> Get '[JSON] [User]

type UserAPI5 = "user" :> Capture "userid" Integer :> Get '[JSON] User
                -- equivalent to 'GET /user/:userid'
                -- except that we explicitly say that "userid"
                -- must be an integer

           :<|> "user" :> Capture "userid" Integer :> DeleteNoContent
                -- equivalent to 'DELETE /user/:userid'

type UserAPI6 = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
                -- equivalent to 'GET /users?sortby={age, name}'

type UserAPI7 = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
                -- - equivalent to 'POST /users' with a JSON object
                --   describing a User in the request body
                -- - returns a User encoded in JSON

           :<|> "users" :> Capture "userid" Integer
                        :> ReqBody '[JSON] User
                        :> Put '[JSON] User
                -- - equivalent to 'PUT /users/:userid' with a JSON
                --   object describing a User in the request body
                -- - returns a User encoded in JSON

type UserAPI8 = "users" :> Header "User-Agent" Text :> Get '[JSON] [User]


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
