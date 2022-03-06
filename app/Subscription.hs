{-# LANGUAGE OverloadedStrings #-}

module Subscription where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    decode,
    object,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Base64.Lazy as LB64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Maybe (mapMaybe)
import qualified Data.Text.Encoding as E
import qualified Data.Yaml as Y
import Network.HTTP.Client.Conduit (parseRequest)
import Network.HTTP.Simple
  ( Response,
    getResponseBody,
    httpLBS,
    parseRequest,
  )
import Network.URI
  ( URI (uriAuthority, uriFragment, uriScheme),
    URIAuth (uriRegName),
    parseURI,
  )

data SSConf = SSConf
  { ssEncryption :: String,
    ssPassword :: String,
    ssIp :: String,
    ssPort :: Int,
    ssName :: String
  }
  deriving (Show)

instance ToJSON SSConf where
  toJSON (SSConf enc pass ip port name) =
    object
      [ "name" .= name,
        "cipher" .= enc,
        "server" .= ip,
        "port" .= port,
        "password" .= pass
      ]

data VMessConf = VMessConf
  { vmessIp :: String,
    vmessPort :: Int,
    vmessId :: String,
    vmessAltid :: Int,
    vmessProtocol :: String, -- unused
    vmessType :: String, -- unused
    vmessTLS :: String, -- unused
    vmessName :: String
  }
  deriving (Show)

instance ToJSON VMessConf where
  toJSON (VMessConf ip port id altid _ _ _ name) =
    object
      [ "name" .= name,
        "type" .= ("vmess" :: String),
        "server" .= ip,
        "port" .= port,
        "uuid" .= id,
        "altId" .= altid,
        "cipher" .= ("auto" :: String)
      ]

instance FromJSON VMessConf where
  parseJSON = withObject "VMessConf" $ \v ->
    VMessConf
      <$> v .: "add"
      <*> (read <$> v .: "port")
      <*> v .: "id"
      <*> v .: "aid"
      <*> v .: "net"
      <*> v .: "type"
      <*> v .: "tls"
      <*> v .: "ps"

data ProxyConf = SS SSConf | VMess VMessConf deriving (Show)

instance ToJSON ProxyConf where
  toJSON (SS conf) = toJSON conf
  toJSON (VMess conf) = toJSON conf

newtype ProxyConfs = ProxyConfs [ProxyConf] deriving (Show)

instance ToJSON ProxyConfs where
  toJSON (ProxyConfs ps) = object ["proxies" .= ps]

parseProxyLink :: URI -> Maybe ProxyConf
parseProxyLink uri =
  case uriScheme uri of
    "ss:" -> parseShadowsocksLink uri
    "vmess:" -> parseVMessLink uri
    _ -> Nothing

parseShadowsocksLink :: URI -> Maybe ProxyConf
parseShadowsocksLink uri = do
  auth <- uriAuthority uri
  let (_ : name) = uriFragment uri
  let raw = LB64.decodeLenient $ LC8.pack $ uriRegName auth
      rawStr = LC8.unpack raw
      (encryption, _ : rest) = span (/= ':') rawStr
      (password, _ : rest') = span (/= '@') rest
      (ip, _ : port) = span (/= ':') rest'
      port' = read port
   in return $ SS $ SSConf encryption password ip port' name

parseVMessLink :: URI -> Maybe ProxyConf
parseVMessLink uri = do
  auth <- uriAuthority uri
  let jsonRaw = LB64.decodeLenient $ LC8.pack $ uriRegName auth
  conf <- decode jsonRaw :: Maybe VMessConf
  return $ VMess conf

parseSubscription :: Response LBS.ByteString -> ProxyConfs
parseSubscription response =
  let body = getResponseBody response
   in case LB64.decode body of
        Left _ -> ProxyConfs []
        Right raw' ->
          let uris = map parseURI $ lines . LC8.unpack $ raw'
           in ProxyConfs $ mapMaybe (>>= parseProxyLink) uris

fetchAndParseSubscription :: String -> IO ProxyConfs
fetchAndParseSubscription subUrl = do
  req <- parseRequest subUrl
  response <- httpLBS req
  return $ parseSubscription response

fetchAndConvertSubscription :: String -> IO String
fetchAndConvertSubscription subUrl = do
  confs <- fetchAndParseSubscription subUrl
  return $ C8.unpack $ Y.encode confs
