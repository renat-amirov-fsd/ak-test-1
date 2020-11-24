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

data Database = Database
  {
    accounts :: [Account],
    tokens :: [Token]
  } deriving (Read, Show, Eq)

data Account = Account
  { accountId :: Int
  , firstName :: String
  , lastName  :: String
  , login     :: String
  , password  :: String
  } deriving (Eq, Show, Read)

data Token = Token
  {  tokenId       :: Int
   , refAccountId  :: Int
   , value         :: String
  } deriving (Eq, Show, Read)


$(deriveJSON defaultOptions ''Account)
$(deriveJSON defaultOptions ''Token)

type API = 
        --       "accounts" :> "all"                           :> Get    '[JSON] [Account]
        --  :<|> "accounts" :> Capture "id" Int                :> Get    '[JSON] Account
          "accounts" :> ReqBody '[JSON] Account         :> Post   '[JSON] Account
      -- :<|> "accounts" :> Capture "accountId" Int         :> Put    '[JSON] Account
      -- :<|> "accounts" :> Capture "accountId" Int         :> DeleteNoContent

      -- :<|> "tokens"   :> "all"                    :> Get    '[JSON] [Token]
      -- :<|> "tokens"   :> Capture "id" Int         :> Get    '[JSON] Token
      -- :<|> "tokens"   :> ReqBody '[JSON] Token    :> Post   '[JSON] Token
      -- :<|> "tokens"   :> Capture "id" Int         :> Put    '[JSON] Token
      -- :<|> "tokens"   :> Capture "id" Int         :> DeleteNoContent

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = 
      --       liftIO   getAllAccounts
      --  :<|> liftIO . getAllAccounts
       liftIO . createAccount
    -- :<|> updateAccountById
    -- :<|> deleteAccountById

    --      getAllTokens
    -- :<|> getTokenById
    -- :<|> createToken
    -- :<|> updateTokenById
    -- :<|> deleteTokenById

  where

    -- getAllAccounts :: IO [Account]
    -- getAllAccounts = do
    --   dbRaw <- readFile "./Database.txt"
    --   let db = read dbRaw
    --   let accts = accounts db
    --   return accts

    -- getAccountById :: Int -> IO Account
    -- getAccountById id = do
    --   dbRaw <- readFile "./Database.txt"
    --   let db = read dbRaw
    --   let acct = head $ filter (\x -> accountId x == id) (accounts db)
    --   return acct

    createAccount :: Account -> IO Account
    createAccount acct = do
      dbRaw <- readFile "./Database.txt"
      let db = read dbRaw
      let updatedDb = db { accounts = acct : accounts db }
      let rawUpdatedDb = show updatedDb
      writeFile "./Database.txt" rawUpdatedDb
      return acct

    -- createAccount id fn ln login psw  = do
    --   dbRaw <- readFile "./Database.txt"
    --   let db = read dbRaw
    --   print db
    --   let updatedDb = db {accounts = newUser : (accounts db)}
    --   let rawUpdatedDb = show updatedDb
    --   writeFile "./Database.txt" rawUpdatedDb
    --   where
    --     newUser = Account { accountId = id, accountFirstName = fn, accountLastName = ln, accountLogin = login, accountPassword = psw }


-- server :: Server API
-- server = liftIO foo
--   where 
--     foo = do
--       insertAccount 1 "qwe" "rty" "uio" "asd"
--       getAccounts

-- server :: Server TokenAPI
-- server = liftIO . getUserToken

-- getAccounts :: IO [Account]
-- getAccounts = do
--   dbRaw <- readFile "./Database.txt"
--   let db = read dbRaw
--   let accts = accounts db
--   return accts

-- getUserToken :: Maybe String -> IO UserToken
-- getUserToken mbLogin = do
--   login <- maybe (error "Error!") pure mbLogin
--   return (generateToken login)

-- generateToken :: String -> UserToken
-- generateToken userId = UserToken userId ((show userId) ++ (take 10 $ repeat 'a'))



