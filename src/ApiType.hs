{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}



module ApiType (app1, kvapp) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.String (IsString, fromString)
import Data.Time.Calendar
import Data.Text                  as T
import Data.Text.Encoding         as T
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Data.Scientific (toBoundedInteger)

-- POST /set
-- {key: "abc", "value": 1123}

-- GET /get
-- {key: "abc"} or {key: 135} or {key: [1,5,7,13]}

type SetEndpoint = 
    "set"
    :> ReqBody '[JSON] KVEntry
    :> Post '[JSON] NoContent

type GetEndpoint = 
    "get"
    :> ReqBody '[JSON] GetRequest
    :> Get '[JSON] KVEntry

data KeyValueType = KVString String | KVInteger Integer deriving (Eq, Show, Generic)

instance FromJSON KeyValueType where
  parseJSON (String s) = pure $ KVString (T.unpack s)
  parseJSON (Number n) = case toBoundedInteger n :: Maybe Int of
                            Just i  -> pure $ KVInteger (toInteger i)
                            Nothing -> mzero
  parseJSON _ = mzero

instance ToJSON KeyValueType where
    toJSON = genericToJSON defaultOptions
    
instance IsString KeyValueType where
  fromString str = KVString str

data KVEntry = KVEntry {
    key :: KeyValueType,
    value :: KeyValueType 
} deriving (Eq, Show, Generic)

instance FromJSON KVEntry
instance ToJSON KVEntry


data GetRequest = GetRequest {
    key :: KeyValueType
} deriving (Eq, Show, Generic)

instance FromJSON GetRequest

type KVAPI = SetEndpoint :<|> GetEndpoint

kvserver :: Server KVAPI
kvserver = serveSet
        :<|> serveGet

    where 
        serveSet :: KVEntry -> Handler NoContent
        serveSet (KVEntry k v) = return NoContent

        serveGet :: GetRequest -> Handler KVEntry
        serveGet (GetRequest (KVString "x")) = return (KVEntry "x" "value_x")
        serveGet (GetRequest (KVString "y")) = return (KVEntry "y" "value_y")
        serveGet (GetRequest other) = return (KVEntry other "unexpected_key")


kvAPI :: Proxy KVAPI
kvAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
kvapp :: Application
kvapp = serve kvAPI kvserver

-- more endpoints:

type UserAPI2 = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

server2 :: Server UserAPI2
server2 = return users2
     :<|> return albert
     :<|> return isaac



-- GET /users?sortby={age,name}
-- return a list sorted by age or name
-- with fields age, name, email, registration_date

-- type UserAPI = 
--     "users" 
--     :> QueryParam "sortby" SortBy 
--     :> Get '[JSON] [User]

-- if we want API with multiple endpoints - use :<|> operator

-- type UserAPI2 = 
--   "users" :> "list-all" :> Get '[JSON] [User]
--   :<|> 
--   "list-all" :> "users" :> Get '[JSON] [User]
-- provides /user/list-all and /list-all/users endpoints


-- Captures allow URL parameters like /users/1, /users/104 etc.

-- type UserAPI5 = "user" :> Capture "userid" Integer :> Get '[JSON] User
                -- equivalent to 'GET /user/:userid'
                -- except that we explicitly say that "userid"
                -- must be an integer

--            :<|> "user" :> Capture "userid" Integer :> DeleteNoContent '[JSON] NoContent
                -- equivalent to 'DELETE /user/:userid'

-- data SortBy = Age | Name

-- data User = User {
--     name :: String,
--     age :: Int,
--     email :: String,
--     registration_date :: UTCTime
-- }


type UserAPI1 = "users" :> Get '[JSON] [User]

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

server1 :: Server UserAPI1
server1 = return users1

userAPI :: Proxy UserAPI1
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server1