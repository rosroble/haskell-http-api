{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs where

-- import Network.HTTP.Types

-- import Web.FormUrlEncoded(FromForm(..), ToForm(..))

import ApiType (GetRequest (..), KVAPI, KVEntry (..), KeyValueType (..))
import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.Wai
import Servant.API
import Servant.Docs
import Servant.Server

instance ToSample KeyValueType where
  toSamples _ = samples [KVString "SomeString", KVInteger 42]

instance ToSample KVEntry where
  toSamples _ = samples [sampleA, sampleB]
    where
      sampleA = KVEntry (KVString "Alex") (KVInteger 22)
      sampleB = KVEntry (KVInteger 1024) (KVString "abc")

instance ToSample GetRequest where
  toSamples _ = singleSample (GetRequest "key_to_lookup")
