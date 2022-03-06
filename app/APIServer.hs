{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module APIServer where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    readTVarIO,
    writeTVar,
  )
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.String (IsString (fromString))
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setHost,
    setPort,
  )
import Servant
  ( Get,
    Handler,
    PlainText,
    Proxy (..),
    QueryParam,
    Server,
    ServerError (errBody),
    err401,
    serve,
    throwError,
    type (:>),
  )
import Servant.API (Get, PlainText, QueryParam, type (:>))
import Subscription (fetchAndConvertSubscription)

type ConfAPI = QueryParam "secret" String :> Get '[PlainText] String -- /?secret=""

confServer :: TVar String -> String -> Server ConfAPI
confServer confVar secret = handler
  where
    checkSecret :: Maybe String -> Bool
    checkSecret maybeSecret =
      null secret
        || ( case maybeSecret of
               Nothing -> False
               Just s' -> s' == secret
           )

    handler :: Maybe String -> Handler String
    handler maybeSecret =
      if checkSecret maybeSecret
        then liftIO $ readTVarIO confVar
        else throwError err401 {errBody = "Sorry sir, wrong secret"}

confAPI :: Proxy ConfAPI
confAPI = Proxy

startAPIServer :: TVar String -> String -> Int -> String -> IO ()
startAPIServer confVar host port secret =
  let app = serve confAPI $ confServer confVar secret
   in let settings = setPort port defaultSettings
       in let settings' = setHost (fromString host) settings
           in runSettings settings' app

oneSec :: Int
oneSec = 1000000

periodicallyFetchWorker :: TVar String -> Int -> String -> IO ()
periodicallyFetchWorker confVar period sublink = forever $ do
  threadDelay $ oneSec * period
  yaml <- fetchAndConvertSubscription sublink
  atomically $ writeTVar confVar yaml
