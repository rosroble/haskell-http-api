{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs () where

-- import Network.HTTP.Types

-- import Web.FormUrlEncoded(FromForm(..), ToForm(..))

import ApiType (GetRequest (..), KVEntry (..), KeyValueType (..))
import Servant.Docs

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
