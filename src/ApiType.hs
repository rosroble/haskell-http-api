{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}



module ApiType (kvapp, KeyValueType, KVAPI) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad (mzero)
import Control.Applicative (empty, (<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TypeScript.TH
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

--- TypeScript Client Gen ---

$(deriveTypeScript defaultOptions ''KeyValueType)
$(deriveTypeScript defaultOptions ''KVEntry)
$(deriveTypeScript defaultOptions ''GetRequest)
$(deriveTypeScript defaultOptions ''NoContent)

------------------------------
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

        serveGet :: GetRequest -> Handler KVEntry
        serveGet (GetRequest k) = do
          map <- liftIO $ readIORef ref
          case Data.Map.lookup k map of
            Just v -> return (KVEntry k v)
            Nothing -> throwError $ err404 {errBody = "Key not found"}

kvAPI :: Proxy KVAPI
kvAPI = Proxy

kvapp :: IORef (Map KeyValueType KeyValueType) -> Application
kvapp ref = serve kvAPI (kvserver ref)