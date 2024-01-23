{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ApiData (module ApiData) where

import GHC.Generics

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
  { name :: String
  }
  deriving (Eq, Show, Generic)

data ListPopResult = ListPopResult
  { success :: Bool,
    error :: String
  }
  deriving (Eq, Show, Generic)

data ListTrimRequest = ListTrimRequest
  { name :: String,
    start :: Int,
    stop :: Int
  }
  deriving (Eq, Show, Generic)

data ListTrimResult = ListTrimResult
  { success :: Bool,
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
