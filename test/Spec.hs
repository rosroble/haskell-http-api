{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import ApiType
import Data.Aeson
import Data.IORef
import Data.Map
import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON as HJSON

main :: IO ()
main = do
  ref <- newIORef (Data.Map.empty :: Map KeyValueType KeyValueType)
  hspec (spec ref)

spec :: IORef (Map KeyValueType KeyValueType) -> Spec
spec ref = with (return (kvapp ref)) $ do
  describe "POST /set" $ do
    let requestBody = encode $ KVEntry (KVString "Alex") (KVInteger 22)
    let requestBodyIncorrect = "{\"key\": \"hi\"}"
    it "responds with 200: correct request body" $ do
      request methodPost "/set" [(hContentType, "application/json")] requestBody `shouldRespondWith` 200
    it "responds with 400: incorrect request body" $ do
      request methodPost "/set" [(hContentType, "application/json")] requestBodyIncorrect `shouldRespondWith` 400
  describe "GET /get" $ do
    let requestBody = encode $ GetRequest (KVString "Alex")
    let requestBodyNoSuchKey = encode $ GetRequest (KVString "not_existing")
    let requestBodyIncorrect = "{\"something\": \"hi\"}"
    it "responds with 200: should return correct key-value" $ do
      request methodGet "/get" [(hContentType, "application/json")] requestBody `shouldRespondWith` [HJSON.json|{key: "Alex", value: 22}|]
    it "responds with 400: bad request body" $ do
      request methodGet "/get" [(hContentType, "application/json")] requestBodyIncorrect `shouldRespondWith` 400
    it "responds with 404: no such key" $ do
      request methodGet "/get" [(hContentType, "application/json")] requestBodyNoSuchKey `shouldRespondWith` 404
