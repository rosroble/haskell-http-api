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
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Serialization() where

import ApiData

import Control.Applicative (empty, many)
import Control.Monad.Except
import Data.Aeson
import Data.HashMap.Strict as HM
import Data.Scientific (toBoundedInteger)
import Data.Text as T hiding (head) 
import Data.Vector as V hiding (head)
import Prelude.Compat
import Text.Read (readMaybe)
import Xmlbf
import qualified Prelude as P

boolToText :: Bool -> Text
boolToText b = if b then "true" else "false"

textToBool :: Text -> Bool
textToBool "true" = True
textToBool "false" = False
textToBool _ = P.error "not convertable to bool"

instance FromJSON KeyValueType where
  parseJSON (String s) = pure $ KVString (unpack s)
  parseJSON (Number n) = case toBoundedInteger n of
    Just i -> pure $ KVInteger i
    Nothing -> mzero
  parseJSON (Array a) = KVList <$> P.mapM parseJSON (V.toList a)
  parseJSON _ = mzero

instance ToJSON KeyValueType where
  toJSON (KVString s) = toJSON s
  toJSON (KVInteger i) = toJSON i
  toJSON (KVList l) = toJSON l

instance FromJSON KVEntry

instance ToJSON KVEntry where
  toJSON (KVEntry k v) = object ["key" .= k, "value" .= v]

instance FromJSON GetRequest

instance ToJSON GetRequest

instance FromXml KeyValueType where
  fromXml =
    pElement "KeyValue" $
      pAttr "type" >>= \t -> case t of
        "string" -> KVString . unpack <$> pText
        "integer" -> KVInteger <$> (pText >>= maybe Control.Applicative.empty pure . Text.Read.readMaybe . unpack)
        "list" -> KVList <$> Control.Applicative.many fromXml
        _ -> Control.Applicative.empty

instance ToXml KeyValueType where
  toXml (KVString s) = text (pack s)
  toXml (KVInteger i) = text (pack (show i))
  toXml (KVList l) = P.concatMap toXml l

instance FromXml GetRequest where
  fromXml = pElement "GetRequest" $ do
    k <- pElement "Key" fromXml
    pure GetRequest {keyToGet = k}

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

instance FromXml StringSetRequest where
  fromXml = pElement "StringSetRequest" $ do
    k <- pElement "StringKey" (T.unpack <$> pText)
    v <- pElement "StringValue" (T.unpack <$> pText)
    pure $ StringSetRequest k v

instance FromXml StringSetNXResult where
  fromXml = pElement "StringSetNXResult" $ do
    ok <- pElement "Success" (textToBool <$> pText)
    pure $ StringSetNXResult ok

instance FromXml StringGetRequest where
  fromXml = pElement "StringGetRequest" $ do
    k <- pElement "StringKey" (T.unpack <$> pText)
    pure $ StringGetRequest k

instance FromXml StringGetResult where
  fromXml = pElement "StringGetRequest" $ do
    ok <- pElement "Success" (textToBool <$> pText)
    v <- pElement "Value" fromXml
    pure $ StringGetResult ok v

instance FromXml StringMGetRequest where 
    fromXml = pElement "StringMGetRequest" $ do
      strkeys <- pElement "StringKeys" (Control.Applicative.many $ T.unpack <$> pText)
      pure $ StringMGetRequest strkeys

instance FromXml StringMGetResult where 
    fromXml = pElement "StringMGetResult" $ do
      strkeys <- pElement "Results" (Control.Applicative.many $ fromXml)
      pure $ StringMGetResult strkeys
  
instance ToXml StringSetRequest where
  toXml (StringSetRequest k v) = 
    element
      "StringSetRequest"
      HM.empty
      [ head $ element "StringKey" HM.empty (text (T.pack k)),
        head $ element "StringValue" HM.empty (text (T.pack v))
      ]

instance ToXml StringSetNXResult where
  toXml (StringSetNXResult ok) = 
    element
      "StringSetNXResult"
      HM.empty
      [ head $ element "Success" HM.empty (text (boolToText ok))
      ]

instance ToXml StringGetRequest where
  toXml (StringGetRequest k) = 
    element
      "StringGetRequest"
      HM.empty
      [ head $ element "StringKey" HM.empty (text (T.pack k))
      ]
      
instance ToXml StringGetResult where
  toXml (StringGetResult ok v) = 
    element
      "StringGetResult"
      HM.empty
      [ head $ element "Success" HM.empty (text (boolToText ok)),
        head $ element "Value" HM.empty (toXml v)
      ]

instance ToXml StringMGetRequest where
  toXml (StringMGetRequest ks) = 
    element
      "StringMGetRequest"
      HM.empty
      (P.concatMap (text . T.pack) ks)

instance ToXml StringMGetResult where
  toXml (StringMGetResult ks) = 
    element
      "StringMGetResult"
      HM.empty
      (P.concatMap toXml ks)

instance FromXml ListPushRequest where
  fromXml = pElement "ListPushRequest" $ do
    name <- pElement "Name" (T.unpack <$> pText)
    v <- pElement "Value" fromXml
    pure $ ListPushRequest name v

instance FromXml ListPushResult where
  fromXml = pElement "ListPushResult" $ do
    ok <- pElement "Success" (textToBool <$> pText)
    err <- pElement "Error" (T.unpack <$> pText)
    pure $ ListPushResult ok err

instance FromXml ListPopRequest where
  fromXml = pElement "ListPopRequest" $ do
    name <- pElement "Name" (T.unpack <$> pText)
    pure $ ListPopRequest name

instance FromXml ListPopResult where
  fromXml = pElement "ListPopResult" $ do
    ok <- pElement "Success" (textToBool <$> pText)
    err <- pElement "Error" (T.unpack <$> pText)
    pure $ ListPopResult ok err

instance FromXml ListTrimRequest where
  fromXml = pElement "ListTrimRequest" $ do
    name <- pElement "Name" (T.unpack <$> pText)
    start <- pElement "Start" (read <$> T.unpack <$> pText)
    stop <- pElement "Stop" (read <$> T.unpack <$> pText)
    pure $ ListTrimRequest name start stop

instance FromXml ListTrimResult where
  fromXml = pElement "ListTrimResult" $ do
    ok <- pElement "Success" (textToBool <$> pText)
    err <- pElement "Error" (T.unpack <$> pText)
    pure $ ListTrimResult ok err

instance FromXml ListLenRequest where
  fromXml = pElement "ListLenRequest" $ do
    name <- pElement "Name" (T.unpack <$> pText)
    pure $ ListLenRequest name

instance FromXml ListLenResult where
  fromXml = pElement "ListLenResult" $ do
    ok <- pElement "Success" (textToBool <$> pText)
    len <- pElement "Len" (read <$> T.unpack <$> pText)
    pure $ ListLenResult ok len

instance ToXml ListPushRequest where
  toXml (ListPushRequest name v) = 
    element
      "ListPushRequest"
      HM.empty
      [ head $ element "Name" HM.empty (text (T.pack name)),
        head $ element "Value" HM.empty (toXml v)
      ]
instance ToXml ListPushResult where
  toXml (ListPushResult ok err) = 
    element
      "ListPushResult"
      HM.empty
      [ head $ element "Success" HM.empty (text (boolToText ok)),
        head $ element "Error" HM.empty (text (T.pack err))
      ]

instance ToXml ListPopRequest where
  toXml (ListPopRequest name) = 
    element
      "ListPopRequest"
      HM.empty
      [ head $ element "Name" HM.empty (text (T.pack name))]

instance ToXml ListPopResult where
  toXml (ListPopResult ok err) = 
    element
      "ListPopResult"
      HM.empty
      [ head $ element "Success" HM.empty (text (boolToText ok)),
        head $ element "Error" HM.empty (text (T.pack err))
      ]

instance ToXml ListTrimRequest where
  toXml (ListTrimRequest name start stop) = 
    element
      "ListPushRequest"
      HM.empty
      [ head $ element "Name" HM.empty (text (T.pack name)),
        head $ element "Start" HM.empty (text (T.pack (show start))),
        head $ element "Stop" HM.empty (text (T.pack (show stop)))
      ]
instance ToXml ListTrimResult where
  toXml (ListTrimResult ok err) = 
    element
      "ListTrimResult"
      HM.empty
      [ head $ element "Success" HM.empty (text (boolToText ok)),
        head $ element "Error" HM.empty (text (T.pack err))
      ]

instance ToXml ListLenRequest where
  toXml (ListLenRequest name) = 
    element
      "ListLenRequest"
      HM.empty
      [ head $ element "Name" HM.empty (text (T.pack name))]

instance ToXml ListLenResult where
  toXml (ListLenResult ok len) = 
    element
      "ListLenResult"
      HM.empty
      [ head $ element "Success" HM.empty (text (boolToText ok)),
        head $ element "Len" HM.empty (text (T.pack (show len)))
      ]


      
instance FromJSON StringSetRequest where
instance FromJSON StringSetNXResult where
instance FromJSON StringGetRequest where
instance FromJSON StringGetResult where
instance FromJSON StringMGetRequest where 
instance FromJSON StringMGetResult where

instance ToJSON StringSetRequest where
instance ToJSON StringSetNXResult where
instance ToJSON StringGetRequest where
instance ToJSON StringGetResult where
instance ToJSON StringMGetRequest where 
instance ToJSON StringMGetResult where

instance ToJSON ListPushRequest where
instance ToJSON ListPushResult where
instance ToJSON ListPopRequest where
instance ToJSON ListPopResult where
instance ToJSON ListTrimRequest where
instance ToJSON ListTrimResult where
instance ToJSON ListLenRequest where
instance ToJSON ListLenResult where

instance FromJSON ListPushRequest where
instance FromJSON ListPushResult where
instance FromJSON ListPopRequest where
instance FromJSON ListPopResult where
instance FromJSON ListTrimRequest where
instance FromJSON ListTrimResult where
instance FromJSON ListLenRequest where
instance FromJSON ListLenResult where