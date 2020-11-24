{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Control.Monad.IO.Class (liftIO)
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
  } deriving (Eq, Show, Read)

data UserToken = UserToken
  { userId     :: String
  , userToken  :: String
  } deriving (Eq, Show, Read)


-- $(deriveJSON defaultOptions ''Account)
$(deriveJSON defaultOptions ''UserToken)

--type API =  "getAccounts"   :> Get  '[JSON] [Account]
      --  :<|> "insertAccount" :> Post '[JSON] [Account]

type TokenAPI = "getToken" :> QueryParam "userid" String :> Get '[JSON] UserToken

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

-- api :: Proxy API
api :: Proxy TokenAPI
api = Proxy

-- server :: Server API
-- server = liftIO foo
--   where 
--     foo = do
--       insertAccount 1 "qwe" "rty" "uio" "asd"
--       getAccounts

server :: Server TokenAPI
server = liftIO . getUserToken

-- getAccounts :: IO [Account]
-- getAccounts = do
--   dbRaw <- readFile "./Database.txt"
--   let db = read dbRaw
--   let accts = accounts db
--   return accts

getUserToken :: Maybe String -> IO UserToken
getUserToken mbLogin = do
  login <- maybe (error "Error!") pure mbLogin
  return (generateToken login)

generateToken :: String -> UserToken
generateToken userId = UserToken userId ((show userId) ++ (take 10 $ repeat 'a'))

data Database = Database
  {
    accounts :: [Account],
    userTokens :: [UserToken]
  } deriving (Read, Show, Eq)


insertAccount id fn ln login psw  = do
  dbRaw <- readFile "./Database.txt"
  let db = read dbRaw
  print db
  let updatedDb = db {accounts = newUser : (accounts db)}
  let rawUpdatedDb = show updatedDb
  writeFile "./Database.txt" rawUpdatedDb
  where
    newUser = Account { accountId = id, accountFirstName = fn, accountLastName = ln, accountLogin = login, accountPassword = psw }