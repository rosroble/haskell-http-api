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



module ApiType (app1, app, kvapp, KeyValueType) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad (mzero)
import Control.Applicative (empty, (<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Map
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
import Text.Read (readMaybe, readEither)
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Data.Scientific (toBoundedInteger)
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Servant.XML
import Xmlbf

-- POST /set
-- {key: "abc", "value": 1123}

-- GET /get
-- {key: "abc"} or {key: 135} or {key: [1,5,7,13]}

type SetEndpoint = 
    "set"
    :> ReqBody '[JSON, XML] KVEntry
    :> Post '[JSON] NoContent

type GetEndpoint = 
    "get"
    :> ReqBody '[JSON, XML] GetRequest
    :> Get '[JSON] KVEntry

data KeyValueType = KVString String | KVInteger Integer deriving (Eq, Show, Generic, Ord)

--- JSON ---

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

--- XML ---

instance FromXml KeyValueType where
  fromXml = fromXmlString <|> fromXmlInteger
    where
      fromXmlString = pElement "KeyValue" $
        pAttr "type" >>= \t -> case t of
          "string" -> KVString . T.unpack <$> pText
          _        -> Control.Applicative.empty

      fromXmlInteger = pElement "KeyValue" $
        pAttr "type" >>= \t -> case t of
          "integer" -> KVInteger <$> (pText >>= maybe Control.Applicative.empty pure . Text.Read.readMaybe . T.unpack)
          _         -> Control.Applicative.empty

----
data KVEntry = KVEntry {
    key :: KeyValueType,
    value :: KeyValueType 
} deriving (Eq, Show, Generic)

instance FromJSON KVEntry
instance ToJSON KVEntry

instance FromXml KVEntry where
  fromXml = pElement "SetRequest" $ do
    k <- pElement "Key" fromXml
    v <- pElement "Value" fromXml
    pure KVEntry{ key = k, value = v}

data GetRequest = GetRequest {
    key :: KeyValueType
} deriving (Eq, Show, Generic)

instance FromJSON GetRequest

instance FromXml GetRequest where
  fromXml = pElement "GetRequest" $ do
    k <- pElement "Key" fromXml
    pure GetRequest{ key = k }

type KVAPI = SetEndpoint :<|> GetEndpoint

kvserver :: IORef (Map KeyValueType KeyValueType) -> Server KVAPI
kvserver ref = serveSet
        :<|> serveGet

    where 
        serveSet :: KVEntry -> Handler NoContent
        serveSet (KVEntry k v) = liftIO $ modifyIORef ref (\m -> Data.Map.insert k v m) >> return NoContent
        -- serveSet (KVEntry k v) = return NoContent

        serveGet :: GetRequest -> Handler KVEntry
        serveGet (GetRequest k) = do
          map <- liftIO $ readIORef ref
          case Data.Map.lookup k map of
            Just v -> return (KVEntry k v)
            Nothing -> throwError $ err404 {errBody = "Key not found"}
        -- serveGet (GetRequest (KVString "x")) = return (KVEntry "x" "value_x")
        -- serveGet (GetRequest (KVString "y")) = return (KVEntry "y" "value_y")
        -- serveGet (GetRequest other) = return (KVEntry other "unexpected_key")


kvAPI :: Proxy KVAPI
kvAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
kvapp :: IORef (Map KeyValueType KeyValueType) -> Application
kvapp ref = serve kvAPI (kvserver ref)




-------------------------------
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

------------- MUTABLE STATE EXAMPLE ----------------

-- Define API: endpoints to get and set the integer
type API = "get" :> Get '[JSON] Int
      :<|> "set" :> ReqBody '[JSON] Int :> PostNoContent

-- Define server handlers
server :: IORef Int -> Server API
server ref = getInt :<|> setInt
    where
    getInt :: Handler Int
    getInt = liftIO $ readIORef ref

    setInt :: Int -> Handler NoContent
    setInt newInt = liftIO $ writeIORef ref newInt >> return NoContent

-- Initialize the application
app :: IORef Int -> Application
app ref = serve (Proxy :: Proxy API) (server ref)