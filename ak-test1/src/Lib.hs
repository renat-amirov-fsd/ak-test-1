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
      accounts :: [Account]
    , tokens :: [Token]
  } deriving (Read, Show, Eq)

data Account = Account
  { accountId :: Int
  , firstName :: String
  , lastName  :: String
  , login     :: String
  , password  :: String
  } deriving (Eq, Show, Read)

data Token = Token
  {  tokenId    :: Int
   , fkAccountId  :: Int
   , tokenValue :: String
  } deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''Account)
$(deriveJSON defaultOptions ''Token)

type API = 
              "accounts" :> "all"                                                  :> Get    '[JSON] [Account]
         :<|> "accounts" :> Capture "id" Int                                       :> Get    '[JSON] Account
         :<|> "accounts" :>                              ReqBody '[JSON] Account   :> Post   '[JSON] Account
      -- :<|> "accounts" :> Capture "id" Int          :> ReqBody '[JSON] Account   :> Put    '[JSON] Account
         :<|> "accounts" :> Capture "id" Int                                       :> Delete '[JSON] ()

         :<|> "tokens"   :> "all"                                                  :> Get    '[JSON] [Token]
         :<|> "tokens"   :> Capture "id" Int                                       :> Get    '[JSON] Token
      -- :<|> "tokens"   :> ReqBody '[JSON] Token                                  :> Post   '[JSON] Token
      -- :<|> "tokens"   :> Capture "id" Int                                       :> Put    '[JSON] Token
      -- :<|> "tokens"   :> Capture "id" Int                                       :> DeleteNoContent

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = 
            liftIO   getAllAccounts
       :<|> liftIO . getAccountById
       :<|> liftIO . createAccount
    -- :<|> updateAccountById
       :<|> liftIO . deleteAccountById

       :<|> liftIO   getAllTokens
       :<|> liftIO . getTokenById
    -- :<|> createToken
    -- :<|> updateTokenById
    -- :<|> deleteTokenById

  where

    -- ACCOUNTS

    getAllAccounts :: IO [Account]
    getAllAccounts = do
      dbRaw <- readFile "./Database.txt"
      let db = read dbRaw
      let accts = accounts db
      return accts

    getAccountById :: Int -> IO Account
    getAccountById id = do
      dbRaw <- readFile "./Database.txt"
      let db = read dbRaw
      let acct = head $ filter (\x -> accountId x == id) (accounts db)
      return acct

    createAccount :: Account -> IO Account
    createAccount acct = do
      print acct
      dbRaw <- readFile "./Database.txt"
      let db = read dbRaw
      print db
      let updatedDb = db { accounts = acct : accounts db }
      let rawUpdatedDb = show updatedDb
      writeFile "./Database.txt" rawUpdatedDb
      print rawUpdatedDb
      return acct

    deleteAccountById :: Int -> IO ()
    deleteAccountById id = do
      dbRaw <- readFile "./Database.txt"
      let db = read dbRaw
      print db
      let updatedDb = db { accounts = filter (\x -> accountId x /= id) (accounts db) }
      let rawUpdatedDb = show updatedDb
      writeFile "./Database.txt" rawUpdatedDb
      print updatedDb
      return ()

    -- TOKENS

    getAllTokens :: IO [Token]
    getAllTokens = do
      dbRaw <- readFile "./Database.txt"
      let db = read dbRaw
      let tkns = tokens db
      return tkns

    getTokenById :: Int -> IO Token
    getTokenById id = do
      dbRaw <- readFile "./Database.txt"
      let db = read dbRaw
      let tkns = head $ filter (\x -> tokenId x == id) (tokens db)
      return tkns


















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



