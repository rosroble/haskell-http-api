{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ApiType (kvapp, kvAPI, KeyValueType (..), KVEntry (..), GetRequest (..), KVAPI) where

import Control.Applicative (empty, (<|>))
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.TypeScript.TH
import Data.HashMap.Strict as HM
import Data.IORef
import Data.Map
import Data.Scientific (toBoundedInteger)
import Data.String (IsString, fromString)
import Data.Text hiding (head)
import GHC.Generics
import Network.Wai
import Prelude.Compat
import Servant
import Servant.XML
import Text.Read (readMaybe)
import Xmlbf
import Prelude ()

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
  parseJSON (String s) = pure $ KVString (unpack s)
  parseJSON (Number n) = case toBoundedInteger n :: Maybe Int of
    Just i -> pure $ KVInteger (toInteger i)
    Nothing -> mzero
  parseJSON _ = mzero

instance ToJSON KeyValueType where
  toJSON (KVString s) = toJSON s
  toJSON (KVInteger i) = toJSON i

instance IsString KeyValueType where
  fromString = KVString

instance FromJSON KVEntry

instance ToJSON KVEntry where
  toJSON (KVEntry k v) = object ["key" .= k, "value" .= v]

instance FromJSON GetRequest

instance ToJSON GetRequest

--- XML ---
instance FromXml KeyValueType where
  fromXml = fromXmlString <|> fromXmlInteger
    where
      fromXmlString =
        pElement "KeyValue" $
          pAttr "type" >>= \t -> case t of
            "string" -> KVString . unpack <$> pText
            _ -> Control.Applicative.empty

      fromXmlInteger =
        pElement "KeyValue" $
          pAttr "type" >>= \t -> case t of
            "integer" -> KVInteger <$> (pText >>= maybe Control.Applicative.empty pure . Text.Read.readMaybe . unpack)
            _ -> Control.Applicative.empty

instance ToXml KeyValueType where
  toXml (KVString s) = text (pack s)
  toXml (KVInteger i) = text (pack (show i))

instance FromXml GetRequest where
  fromXml = pElement "GetRequest" $ do
    k <- pElement "Key" fromXml
    pure GetRequest {key = k}

instance ToXml GetRequest where
  toXml (GetRequest k) =
    [ head $
        element
          "GetRequest"
          HM.empty
          [ head $ element "Key" HM.empty (toXml k)
          ]
    ]

instance FromXml KVEntry where
  fromXml = pElement "SetRequest" $ do
    k <- pElement "Key" fromXml
    v <- pElement "Value" fromXml
    pure KVEntry {key = k, value = v}

instance ToXml KVEntry where
  toXml (KVEntry k v) =
    element
      "KVEntry"
      HM.empty
      [ head $ element "Key" HM.empty (toXml k),
        head $ element "Value" HM.empty (toXml v)
      ]

----
data KVEntry = KVEntry
  { key :: KeyValueType,
    value :: KeyValueType
  }
  deriving (Eq, Show, Generic)

data GetRequest = GetRequest
  { key :: KeyValueType
  }
  deriving (Eq, Show, Generic)

--- TypeScript Client Gen ---

$(deriveTypeScript defaultOptions ''KeyValueType)
$(deriveTypeScript defaultOptions ''KVEntry)
$(deriveTypeScript defaultOptions ''GetRequest)
$(deriveTypeScript defaultOptions ''NoContent)

------------------------------

type KVAPI = SetEndpoint :<|> GetEndpoint

kvserver :: IORef (Map KeyValueType KeyValueType) -> Server KVAPI
kvserver ref =
  serveSet
    :<|> serveGet
  where
    serveSet :: KVEntry -> Handler NoContent
    serveSet (KVEntry k v) = liftIO $ modifyIORef ref (Data.Map.insert k v) >> return NoContent

    serveGet :: GetRequest -> Handler KVEntry
    serveGet (GetRequest k) = do
      mp <- liftIO $ readIORef ref
      case Data.Map.lookup k mp of
        Just v -> return (KVEntry k v)
        Nothing -> throwError $ err404 {errBody = "Key not found"}

kvAPI :: Proxy KVAPI
kvAPI = Proxy

kvapp :: IORef (Map KeyValueType KeyValueType) -> Application
kvapp ref = serve kvAPI (kvserver ref)
