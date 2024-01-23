{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs (docAsString) where

import ApiData
import Data.String (IsString (..))
import Servant
import Servant.Docs

instance IsString KeyValueType where
  fromString = KVString

instance ToSample Char where
  toSamples _ = samples ['a', 'b', 'c']

instance ToSample Int where
  toSamples _ = samples [1, 2, 3]

instance ToSample Integer where
  toSamples _ = samples [1, 2, 3]

instance ToSample KeyValueType where
  toSamples _ = samples [KVString "SomeString", KVInteger 42]

instance ToSample KVEntry where
  toSamples _ = samples [sampleA, sampleB, sampleC]
    where
      sampleA = KVEntry (KVString "Alex") (KVInteger 22)
      sampleB = KVEntry (KVInteger 1024) (KVString "abc")
      sampleC = KVEntry (KVList [KVInteger 1, KVString "str"]) (KVInteger 1002)

instance ToSample GetRequest where
  toSamples _ = singleSample (GetRequest "key_to_lookup")

instance ToSample StringSetRequest where
  toSamples _ = singleSample (StringSetRequest "some_str_key" "some_str_val")

instance ToSample StringSetNXResult where
  toSamples _ = singleSample (StringSetNXResult True)

instance ToSample StringGetRequest where
  toSamples _ = singleSample (StringGetRequest "some_str_key")

instance ToSample StringGetResult where
  toSamples _ = singleSample (StringGetResult True (KVString "some_str_key"))

instance ToSample StringMGetRequest where
  toSamples _ = singleSample (StringMGetRequest ["some_str_key", "another_str_key"])

instance ToSample StringMGetResult where
  toSamples _ = singleSample $ StringMGetResult [StringGetResult True (KVString "some_str_key"), StringGetResult True (KVString "another_str_key")]

instance ToSample ListPushRequest where
  toSamples _ = singleSample $ ListPushRequest "some_str_key" (KVInteger 3)

instance ToSample ListPushResult where
  toSamples _ = singleSample (ListPushResult False "error")

instance ToSample ListPopRequest where
  toSamples _ = singleSample (ListPopRequest "somelist")

instance ToSample ListPopResult where
  toSamples _ = singleSample (ListPopResult True "")

instance ToSample ListTrimRequest where
  toSamples _ = singleSample (ListTrimRequest "list" 1 5)

instance ToSample ListTrimResult where
  toSamples _ = singleSample (ListTrimResult True "")

instance ToSample ListLenRequest where
  toSamples _ = singleSample (ListLenRequest "listname")

instance ToSample ListLenResult where
  toSamples _ = singleSample (ListLenResult True 15)

docAsString :: (HasDocs a) => Proxy a -> String
docAsString api = markdown (docs api)
