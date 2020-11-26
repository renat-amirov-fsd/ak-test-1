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

-- import Data.Time.Clock
-- import Data.Time.Calendar
import System.Random

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
         :<|> "accounts" :> Capture "id" Int          :> ReqBody '[JSON] Account   :> Put    '[JSON] Account 
         :<|> "accounts" :> Capture "id" Int                                       :> Delete '[JSON] ()

         :<|> "tokens"   :> "all"                                                  :> Get    '[JSON] [Token]
         :<|> "tokens"   :> Capture "id" Int                                       :> Get    '[JSON] Token
         :<|> "tokens"   :>                              ReqBody '[JSON] Token     :> Post   '[JSON] Token
         :<|> "tokens"   :> QueryParam "login" String :> QueryParam "psw" String   :> Get    '[JSON] Token
         :<|> "tokens"   :> Capture "id" Int          :> ReqBody '[JSON] Token     :> Put    '[JSON] Token
         :<|> "tokens"   :> Capture "id" Int                                       :> Delete '[JSON] ()

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
       :<|> (\id acct -> liftIO $ updateAccountById id acct)
       :<|> liftIO . deleteAccountById

       :<|> liftIO   getAllTokens
       :<|> liftIO . getTokenById
       :<|> liftIO . createToken
       :<|> (\login psw -> liftIO $ createTokenByLoginAndPsw login psw)
       :<|> (\id tkn -> liftIO $ updateTokenById id tkn)
       :<|> liftIO . deleteTokenById

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

    updateAccountById :: Int -> Account -> IO Account
    updateAccountById id acct = do
      print acct
      dbRaw <- readFile "./Database.txt"
      let db = read dbRaw
      print db
      let updatedDb = db { accounts = acct : filter (\x -> accountId x /= id) (accounts db) }
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

    createToken :: Token -> IO Token
    createToken tkn = do
      print tkn
      dbRaw <- readFile "./Database.txt"
      let db = read dbRaw
      print db
      let updatedDb = db { tokens = tkn : tokens db }
      let rawUpdatedDb = show updatedDb
      writeFile "./Database.txt" rawUpdatedDb
      print rawUpdatedDb
      return tkn

    createTokenByLoginAndPsw :: Maybe String -> Maybe String -> IO Token
    createTokenByLoginAndPsw mbLgn mbPsw = do
      lgn <- maybe (error "Error in login!") pure mbLgn
      psw <- maybe (error "Error in psw!") pure mbPsw
      dbRaw <- readFile "./Database.txt"
      let db = read dbRaw
      print db
      let acct = head $ filter (\x -> login x == lgn && password x == psw) (accounts db)
      let acctId = accountId acct
      --let tkn = random (mkStdGen 100)
      let tkn = Token acctId acctId (show acctId ++ (take 10 $ repeat 'a') ++ show (fst (random (mkStdGen 100) :: (Int, StdGen))))
      let updatedDb = db { tokens = tkn : tokens db }
      let rawUpdatedDb = show updatedDb
      writeFile "./Database.txt" rawUpdatedDb
      print rawUpdatedDb
      return tkn

    updateTokenById :: Int -> Token -> IO Token
    updateTokenById id tkn = do
      print tkn
      dbRaw <- readFile "./Database.txt"
      let db = read dbRaw
      print db
      let updatedDb = db { tokens = tkn : filter (\x -> tokenId x /= id) (tokens db) }
      let rawUpdatedDb = show updatedDb
      writeFile "./Database.txt" rawUpdatedDb
      print rawUpdatedDb
      return tkn

    deleteTokenById :: Int -> IO ()
    deleteTokenById id = do
      dbRaw <- readFile "./Database.txt"
      let db = read dbRaw
      print db
      let updatedDb = db { tokens = filter (\x -> tokenId x /= id) (tokens db) }
      let rawUpdatedDb = show updatedDb
      writeFile "./Database.txt" rawUpdatedDb
      print updatedDb
      return ()

-- date :: IO (Integer,Int,Int) -- :: (year,month,day)
-- date = getCurrentTime >>= return . toGregorian . utctDay

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



