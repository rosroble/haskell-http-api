{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module ApiData(module ApiData) where

import Control.Applicative (empty, many)
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.TypeScript.TH
import Data.HashMap.Strict as HM
import Data.IORef
import Data.Map
import Data.Scientific (toBoundedInteger)
import Data.String (IsString, fromString)
import Data.Text hiding (head)
import Data.Vector as V hiding (head)
import GHC.Generics
import Network.Wai
import Prelude.Compat
import Servant
import Servant.XML
import Text.Read (readMaybe)
import Xmlbf
import qualified Prelude as P

data KeyValueType = KVString String | KVInteger Int | KVList [KeyValueType] deriving (Eq, Show, Generic, Ord)

data KVEntry = KVEntry
  { key :: KeyValueType,
    value :: KeyValueType
  }
  deriving (Eq, Show, Generic)

data GetRequest = GetRequest
  { keyToGet :: KeyValueType
  }
  deriving (Eq, Show, Generic)

data StringSetRequest = StringSetRequest
  { stringKey :: String,
    stringValue :: String
  }
  deriving (Eq, Show, Generic)

data StringSetNXResult = StringSetNXResult
  { success :: Bool
  }
  deriving (Eq, Show, Generic)

data StringGetRequest = StringGetRequest
  { stringKey :: String
  }
  deriving (Eq, Show, Generic)

data StringGetResult = StringGetResult
  { success :: Bool,
    value :: KeyValueType
  }
  deriving (Eq, Show, Generic)

data StringMGetRequest = StringMGetRequest
  { stringKeys :: [String]
  }
  deriving (Eq, Show, Generic)

data StringMGetResult = StringMGetResult
  { results :: [StringGetResult]
  }
  deriving (Eq, Show, Generic)

data ListPushRequest = ListPushRequest
  { name :: String,
    value :: KeyValueType  
  }
  deriving (Eq, Show, Generic)

data ListPushResult = ListPushResult
  { success :: Bool,
    error :: String 
  }
  deriving (Eq, Show, Generic)

data ListPopRequest = ListPopRequest
 {  name :: String
 }
  deriving (Eq, Show, Generic)

data ListPopResult = ListPopResult
 {   success :: Bool,
     error :: String
 }
  deriving (Eq, Show, Generic)

data ListTrimRequest = ListTrimRequest
 {   name :: String,
    start :: Int,
    stop :: Int
 }
  deriving (Eq, Show, Generic) 

data ListTrimResult = ListTrimResult
 {   success :: Bool,
    error :: String
 }
  deriving (Eq, Show, Generic)

data ListLenRequest = ListLenRequest
 { name :: String
 }
  deriving (Eq, Show, Generic)

data ListLenResult = ListLenResult
 { success :: Bool,
   len :: Int
 }
  deriving (Eq, Show, Generic)
