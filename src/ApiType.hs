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

import ApiData
import Serialization()
import Docs
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.TypeScript.TH
import Data.IORef
import Data.Map
import Network.Wai
import Prelude.Compat
import Servant
import Servant.XML
import qualified Prelude as P

type SetEndpoint =
  "set"
    :> ReqBody '[JSON, XML] KVEntry
    :> Post '[JSON] NoContent

type GetEndpoint =
  "get"
    :> ReqBody '[JSON, XML] GetRequest
    :> Get '[JSON] KVEntry

type StringSetEndpoint = 
  "strings" :> "set"
    :> ReqBody '[JSON, XML] StringSetRequest
    :> Post '[JSON] NoContent

type StringSetNXEndpoint = 
  "strings" :> "setnx"
    :> ReqBody '[JSON, XML] StringSetRequest
    :> Post '[JSON] StringSetNXResult

type StringGetEndpoint = 
  "strings" :> "get"
    :> ReqBody '[JSON, XML] StringGetRequest
    :> Get '[JSON] StringGetResult

type StringMGetEndpoint =
  "strings" :> "mget"
    :> ReqBody '[JSON, XML] StringMGetRequest
    :> Get '[JSON] StringMGetResult

type ListLPushEndpoint = 
  "lists" :> "lpush"
    :> ReqBody '[JSON, XML] ListPushRequest
    :> Post '[JSON] ListPushResult

type ListRPushEndpoint = 
  "lists" :> "rpush"
    :> ReqBody '[JSON, XML] ListPushRequest
    :> Post '[JSON] ListPushResult

type ListLPopEndpoint = 
  "lists" :> "lpop"
    :> ReqBody '[JSON, XML] ListPopRequest
    :> Post '[JSON] ListPopResult

type ListRPopEndpoint = 
  "lists" :> "rpop"
    :> ReqBody '[JSON, XML] ListPopRequest
    :> Post '[JSON] ListPopResult

type ListLenEndpoint = 
  "lists" :> "len"
    :> ReqBody '[JSON, XML] ListLenRequest
    :> Get '[JSON] ListLenResult

type ListTrimEndpoint = 
  "lists" :> "trim"
    :> ReqBody '[JSON, XML] ListTrimRequest
    :> Post '[JSON] ListTrimResult

type DocsEndpoint = 
  "docs"
    :> Get '[JSON, PlainText] String

--- TypeScript Client Gen ---

$(deriveTypeScript defaultOptions ''KeyValueType)
$(deriveTypeScript defaultOptions ''KVEntry)
$(deriveTypeScript defaultOptions ''GetRequest)
$(deriveTypeScript defaultOptions ''NoContent)
$(deriveTypeScript defaultOptions ''StringSetRequest) 
$(deriveTypeScript defaultOptions ''StringSetNXResult) 
$(deriveTypeScript defaultOptions ''StringGetRequest) 
$(deriveTypeScript defaultOptions ''StringGetResult) 
$(deriveTypeScript defaultOptions ''StringMGetRequest) 
$(deriveTypeScript defaultOptions ''StringMGetResult)
$(deriveTypeScript defaultOptions ''ListPushRequest)
$(deriveTypeScript defaultOptions ''ListPushResult)
$(deriveTypeScript defaultOptions ''ListPopRequest)
$(deriveTypeScript defaultOptions ''ListPopResult)
$(deriveTypeScript defaultOptions ''ListTrimRequest)
$(deriveTypeScript defaultOptions ''ListTrimResult)
$(deriveTypeScript defaultOptions ''ListLenRequest)
$(deriveTypeScript defaultOptions ''ListLenResult)
------------------------------

type KVAPI = SetEndpoint 
  :<|> GetEndpoint 
  :<|> StringSetEndpoint 
  :<|> StringSetNXEndpoint 
  :<|> StringGetEndpoint 
  :<|> StringMGetEndpoint
  :<|> ListLPushEndpoint
  :<|> ListRPushEndpoint
  :<|> ListLPopEndpoint
  :<|> ListRPopEndpoint
  :<|> ListLenEndpoint
  :<|> ListTrimEndpoint
  :<|> DocsEndpoint

kvserver :: IORef (Map KeyValueType KeyValueType) -> Server KVAPI
kvserver ref =
  serveSet
    :<|> serveGet
    :<|> serveStringSet
    :<|> serveStringSetNX
    :<|> serveStringGet
    :<|> serveStringMGet
    :<|> serveListLPush
    :<|> serveListRPush
    :<|> serveListLPop
    :<|> serveListRPop
    :<|> serveListLen
    :<|> serveListTrim
    :<|> serveDocs

  where
    serveSet :: KVEntry -> Handler NoContent
    serveSet (KVEntry k v) = liftIO $ modifyIORef ref (Data.Map.insert k v) >> return NoContent

    serveGet :: GetRequest -> Handler KVEntry
    serveGet (GetRequest k) = do
      mp <- liftIO $ readIORef ref
      case Data.Map.lookup k mp of
        Just v -> return (KVEntry k v)
        Nothing -> throwError $ err404 {errBody = "Key not found"}
    
    serveStringSet :: StringSetRequest -> Handler NoContent
    serveStringSet (StringSetRequest k v) = liftIO $ modifyIORef ref (Data.Map.insert (KVString k) (KVString v)) >> return NoContent

    serveStringSetNX :: StringSetRequest -> Handler StringSetNXResult
    serveStringSetNX (StringSetRequest k v) = do
      mp <- liftIO $ readIORef ref
      case Data.Map.lookup (KVString k) mp of
        Just _ -> throwError $ err403 {errBody = Data.Aeson.encode $ StringSetNXResult {success = False}}
        Nothing -> liftIO $ modifyIORef ref (Data.Map.insert (KVString k) (KVString v)) >> return StringSetNXResult {success = True}

    serveStringGet :: StringGetRequest -> Handler StringGetResult
    serveStringGet (StringGetRequest k) = do
      mp <- liftIO $ readIORef ref
      case Data.Map.lookup (KVString k) mp of
        Just v -> return (StringGetResult True v)
        Nothing -> throwError $ err404 {errBody = Data.Aeson.encode $ StringGetResult False ""}

    serveStringMGet :: StringMGetRequest -> Handler StringMGetResult
    serveStringMGet (StringMGetRequest ks) = do
      mp <- liftIO $ readIORef ref
      return $ StringMGetResult (lookupMultiple mp ks)
    
    serveListLPush :: ListPushRequest -> Handler ListPushResult
    serveListLPush = listPushHandler True

    serveListRPush :: ListPushRequest -> Handler ListPushResult
    serveListRPush = listPushHandler False

    serveListLPop :: ListPopRequest -> Handler ListPopResult
    serveListLPop = listPopHandler True

    serveListRPop :: ListPopRequest -> Handler ListPopResult
    serveListRPop = listPopHandler False

    serveListLen :: ListLenRequest -> Handler ListLenResult
    serveListLen (ListLenRequest listName) = testListLength listName

    serveListTrim :: ListTrimRequest -> Handler ListTrimResult
    serveListTrim = listTrim

    serveDocs :: Handler String
    serveDocs = return $ docAsString kvAPI

    listPushHandler :: Bool -> ListPushRequest -> Handler ListPushResult
    listPushHandler isLeft req = do
      pushRes <- listPush isLeft req
      case pushRes of
        ListPushResult True _ -> return pushRes
        ListPushResult _ _ -> throwError $ err400 {errBody = Data.Aeson.encode pushRes}
    
    listPopHandler :: Bool -> ListPopRequest -> Handler ListPopResult
    listPopHandler isLeft (ListPopRequest listName) = do
      listLen <- (testListLength listName) >>= return . len
      if listLen < 1 then return $ ListPopResult False "nothing to pop" else do
        popLeft <- return $ fromEnum isLeft
        popRight <- return $ listLen - (fromEnum $ not isLeft)
        popRes <- listTrim (ListTrimRequest listName popLeft popRight)
        case popRes of
          ListTrimResult True _ -> return $ ListPopResult True ""
          ListTrimResult _  err -> throwError $ err400 {errBody = Data.Aeson.encode $ ListPopResult False err}

    listPush :: Bool -> ListPushRequest -> Handler ListPushResult
    listPush isLeft (ListPushRequest listName val) = do
      mp <- liftIO $ readIORef ref
      case Data.Map.lookup (KVString listName) mp of
        Just (KVList l) -> liftIO $ modifyIORef ref (Data.Map.insert (KVString listName) (KVList $ appendOrPrepend isLeft l val)) >> return ListPushResult {success = True, error = ""}
        Nothing -> liftIO $ modifyIORef ref (Data.Map.insert (KVString listName) (KVList $ appendOrPrepend isLeft [] val)) >> return ListPushResult {success = True, error = ""}
        _ -> return $ ListPushResult False "not a list"
    
    listTrim :: ListTrimRequest -> Handler ListTrimResult
    listTrim (ListTrimRequest listName fromIdx toIdx) = do
        mp <- liftIO $ readIORef ref
        case Data.Map.lookup (KVString listName) mp of
          Just (KVList l) -> liftIO $ modifyIORef ref (Data.Map.insert (KVString listName) (KVList $ trim fromIdx toIdx l)) >> return ListTrimResult {success = True, error = ""}
          _ -> return $ ListTrimResult False "not a list"

    testListLength :: String -> Handler ListLenResult
    testListLength listName = do
      mp <- liftIO $ readIORef ref
      case Data.Map.lookup (KVString listName) mp of
        Just (KVList l) -> return $ ListLenResult True (P.length l)
        _ -> return $ ListLenResult False (-1)

    -- handle bad indexes?
    trim :: Int -> Int -> [a] -> [a]
    trim fromIdx toIdx xs = P.take (toIdx - fromIdx) (P.drop fromIdx xs)

    -- Bool = isLeft (prepend)
    appendOrPrepend :: Bool -> [KeyValueType] -> KeyValueType -> [KeyValueType]
    appendOrPrepend True lst new = new:lst 
    appendOrPrepend False lst new = lst P.++ [new]

    lookupMultiple :: Data.Map.Map KeyValueType KeyValueType -> [String] -> [StringGetResult]
    lookupMultiple mp ks = P.map (lookupToStringGetResult mp) ks

    lookupToStringGetResult :: Data.Map.Map KeyValueType KeyValueType -> String -> StringGetResult
    lookupToStringGetResult mp k = 
      case Data.Map.lookup (KVString k) mp of
        Just v -> StringGetResult True v
        Nothing -> StringGetResult False ""  

kvAPI :: Proxy KVAPI
kvAPI = Proxy

kvapp :: IORef (Map KeyValueType KeyValueType) -> Application
kvapp ref = serve kvAPI (kvserver ref)
