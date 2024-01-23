{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs (docAsString) where

import ApiData
import Data.String (IsString(..))
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
instance ToSample StringSetNXResult where
instance ToSample StringGetRequest where
instance ToSample StringGetResult where
instance ToSample StringMGetRequest where 
instance ToSample StringMGetResult where

instance ToSample ListPushRequest where
instance ToSample ListPushResult where
instance ToSample ListPopRequest where
instance ToSample ListPopResult where
instance ToSample ListTrimRequest where
instance ToSample ListTrimResult where
instance ToSample ListLenRequest where
instance ToSample ListLenResult where

docAsString :: HasDocs a => Proxy a -> String
docAsString api = markdown (docs api)