{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

-- import System.Directory (getDirectoryContents, )
-- import Text.CSV
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

-- data Account = Account
--   { accountId        :: String
--   , accountLogin     :: String
--   } deriving (Eq, Show)

data Account = Account
  { accountId        :: Int
  , accountFirstName :: String
  , accountLastName  :: String
  , accountLogin :: String
  , accountPassword  :: String
  } deriving (Eq, Show, Read)


$(deriveJSON defaultOptions ''Account)

type API =  "getAccounts"   :> Get  '[JSON] [Account]
      --  :<|> "insertAccount" :> Post '[JSON] [Account]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = liftIO foo
  where 
    foo = do
      insertAccount 1 "qwe" "rty" "uio" "asd"
      getAccounts

getAccounts :: IO [Account]
getAccounts = do
  dbRaw <- readFile "./Database.txt"
  let db = read dbRaw
  let accts = accounts db
  return accts

data Database = Database
  {
    accounts :: [Account]
    -- userTokens :: [UserToken]
  } deriving (Read, Show, Eq)


insertAccount id fn ln login psw  = do
  dbRaw <- readFile "./Database.txt"
  let db = read dbRaw
  let updatedDb = db {accounts = newUser : (accounts db)}
  let rawUpdatedDb = show updatedDb
  writeFile "./Database.txt" rawUpdatedDb
  where
    newUser = Account { accountId = id, accountFirstName = fn, accountLastName = ln, accountLogin = login, accountPassword = psw }


-- mapify :: [String] -> [Account]
-- mapify []      = []
-- mapify (x:xs)  = Account id login : mapify xs
--   where
--     w = words x
--     id = head w
--     login = last w
    




-- ls <- fmap Text.lines (Text.readFile "filename.txt")

-- filecontent <- liftIO (readFile "myfile.txt")
--   return (FileContent filecontent)

-- getAccounts = [ Account 1 "Isaac" "Newton" "isya" "qwerty"
--            , Account 2 "Albert" "Einstein" "alb" "asdfgh"
--            ]


-- readFile :: FilePath -> IO String
-- The readFile function reads a file and returns the contents of the file as a string. 
-- The file is read lazily, on demand, as with getContents.

-- main::IO()
-- main = do
--        dbSales <- readFile "la.txt"
--        let sales = lines dbSales
--        (result, x, y) <- mapify sales
--        print result

-- mapify :: [String] -> Map Int Int
-- mapify = Prelude.foldr (\s m -> let (id:count:desc) = (splitWhen (=='|') s)
--                                     i = read id
--                                     c = read count
--                                  in insertWith (+) i c m) empty


