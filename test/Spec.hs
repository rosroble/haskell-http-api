{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import ApiType
import ApiData
import Data.Aeson as A
import Data.IORef
import Data.Map
import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON as HJSON
import Prelude as P

main :: IO ()
main = do
  ref <- newIORef (Data.Map.empty :: Map KeyValueType KeyValueType)
  hspec (spec ref)

spec :: IORef (Map KeyValueType KeyValueType) -> Spec
spec ref = with (return (kvapp ref)) $ do
  describe "POST /set" $ do
    let requestBody = A.encode $ KVEntry (KVString "Alex") (KVInteger 22)
    let requestBodyIncorrect = "{\"key\": \"hi\"}"
    it "responds with 200: correct request body" $ do
      request methodPost "/set" [(hContentType, "application/json")] requestBody `shouldRespondWith` 200
    it "responds with 400: incorrect request body" $ do
      request methodPost "/set" [(hContentType, "application/json")] requestBodyIncorrect `shouldRespondWith` 400
  describe "GET /get" $ do
    let requestBody = A.encode $ GetRequest (KVString "Alex")
    let requestBodyNoSuchKey = A.encode $ GetRequest (KVString "not_existing")
    let requestBodyIncorrect = "{\"something\": \"hi\"}"
    it "responds with 200: should return correct key-value" $ do
      request methodGet "/get" [(hContentType, "application/json")] requestBody `shouldRespondWith` [HJSON.json|{key: "Alex", value: 22}|]
    it "responds with 400: bad request body" $ do
      request methodGet "/get" [(hContentType, "application/json")] requestBodyIncorrect `shouldRespondWith` 400
    it "responds with 404: no such key" $ do
      request methodGet "/get" [(hContentType, "application/json")] requestBodyNoSuchKey `shouldRespondWith` 404
  describe "POST /strings/set" $ do
    let requestBody = A.encode $ StringSetRequest "some_key" "some_val"
    it "responds with 200" $ do
      request methodPost "/strings/set" [(hContentType, "application/json")] requestBody `shouldRespondWith` 200
  describe "POST /strings/setnx" $ do
    let requestBody = A.encode $ StringSetRequest "nx_key" "nx_val"
    let requestBodyKeyExists = A.encode $ StringSetRequest "some_key" "another_val"
    it "responds with 200" $ do
      request methodPost "/strings/setnx" [(hContentType, "application/json")] requestBody `shouldRespondWith` 200
    it "responds with 403: key already exists" $ do
      request methodPost "/strings/setnx" [(hContentType, "application/json")] requestBodyKeyExists `shouldRespondWith` 403
  describe "GET /strings/get" $ do
    let requestBody = A.encode $ StringGetRequest "some_key"
    let requestBodyKeyNotExists = A.encode $ StringGetRequest "nosuchkey"
    it "responds with 200" $ do
      request methodGet "/strings/get" [(hContentType, "application/json")] requestBody `shouldRespondWith` [HJSON.json|{success: true, value: "some_val"}|]
    it "responds with 404: key does not exist" $ do
      request methodGet "/strings/get" [(hContentType, "application/json")] requestBodyKeyNotExists `shouldRespondWith` 404
  describe "GET /strings/mget" $ do
    let requestBody = A.encode $ StringMGetRequest ["some_key", "nx_key"]
    it "responds with 200" $ do
      request methodGet "/strings/mget" [(hContentType, "application/json")] requestBody `shouldRespondWith` [HJSON.json|{results: [{success: true, value: "some_val"}, {success: true, value: "nx_val"}]}|]
  describe "POST /lists/[l,r]push" $ do
    let requestBodyRight1 = A.encode $ ListPushRequest "somelist" (KVInteger 10)
    let requestBodyRight2 = A.encode $ ListPushRequest "somelist" (KVInteger 19)
    let requestBodyRight3 = A.encode $ ListPushRequest "somelist" (KVInteger 41)
    let requestBodyLeft1 = A.encode $ ListPushRequest "somelist" (KVInteger 30)
    let requestBodyLeft2 = A.encode $ ListPushRequest "somelist" (KVInteger 349)
    let requestBodyLeft3 = A.encode $ ListPushRequest "somelist" (KVInteger 1010)
    it "responds with 200: rpush1" $ do
      request methodPost "/lists/rpush" [(hContentType, "application/json")] requestBodyRight1 `shouldRespondWith` 200
    it "responds with 200: rpush2" $ do
      request methodPost "/lists/rpush" [(hContentType, "application/json")] requestBodyRight2 `shouldRespondWith` 200
    it "responds with 200: rpush3" $ do
      request methodPost "/lists/rpush" [(hContentType, "application/json")] requestBodyRight3 `shouldRespondWith` 200
    it "responds with 200: lpush1" $ do
      request methodPost "/lists/lpush" [(hContentType, "application/json")] requestBodyLeft1 `shouldRespondWith` 200
    it "responds with 200: lpush2" $ do
      request methodPost "/lists/lpush" [(hContentType, "application/json")] requestBodyLeft2 `shouldRespondWith` 200
    it "responds with 200: lpush3" $ do
      request methodPost "/lists/lpush" [(hContentType, "application/json")] requestBodyLeft3 `shouldRespondWith` 200
  describe "POST /lists/[l,r]pop" $ do
    let requestBody = A.encode $ ListPopRequest "somelist"
    let requestBodyGet = A.encode $ GetRequest (KVString "somelist")
    it "responds with 200: lpop" $ do
      request methodPost "/lists/lpop" [(hContentType, "application/json")] requestBody `shouldRespondWith` 200
    it "responds with 200: rpop" $ do
      request methodPost "/lists/rpop" [(hContentType, "application/json")] requestBody `shouldRespondWith` 200
    it "responds with 200: should return correct list state" $ do
      request methodGet "/get" [(hContentType, "application/json")] requestBodyGet `shouldRespondWith` [HJSON.json|{key: "somelist", value: [349, 30, 10, 19]}|]
  describe "POST /lists/trim" $ do
    let requestBody = A.encode $ ListTrimRequest "somelist" 1 3
    let requestBodyGet = A.encode $ GetRequest (KVString "somelist")
    it "responds with 200: trim" $ do
      request methodPost "/lists/trim" [(hContentType, "application/json")] requestBody `shouldRespondWith` 200
    it "responds with 200: should return correct list state" $ do
      request methodGet "/get" [(hContentType, "application/json")] requestBodyGet `shouldRespondWith` [HJSON.json|{key: "somelist", value: [30, 10]}|]
  describe "GET /lists/len" $ do
    let requestBody = A.encode $ ListLenRequest "somelist"
    it "responds with 200: len" $ do
      request methodGet "/lists/len" [(hContentType, "application/json")] requestBody `shouldRespondWith` [HJSON.json|{success: true, len: 2}|]