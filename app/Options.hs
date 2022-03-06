{-# LANGUAGE ApplicativeDo #-}

module Options where

import Data.Semigroup ((<>))
import Options.Applicative
  ( Alternative ((<|>)),
    Parser,
    auto,
    execParser,
    flag',
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    strOption,
    switch,
    value,
    (<**>),
  )
import Subscription (fetchAndParseSubscription)

data Output
  = FileOutput FilePath
  | StdOutput
  deriving (Show)

fileOutput :: Parser Output
fileOutput =
  FileOutput
    <$> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILENAME"
          <> help "Output file(default: out.yaml)"
          <> value "out.yaml"
      )

stdOutput :: Parser Output
stdOutput =
  flag'
    StdOutput
    ( long "stdout"
        <> help "Print to stdout"
    )

output :: Parser Output
output = fileOutput <|> stdOutput

newtype SubscriptionLink = SubscriptionLink String deriving (Show)

sublink :: Parser SubscriptionLink
sublink =
  SubscriptionLink
    <$> strOption
      ( long "subscription"
          <> short 's'
          <> metavar "URL"
          <> help "Just my socks subscription link"
      )

enableAPIServer :: Parser ()
enableAPIServer =
  flag'
    ()
    ( long "enable-api-server"
        <> short 'w'
        <> help "Enable API server"
    )

data APIServerOptions = APIServerOptions
  { apiServerListenAddr :: String,
    apiServerListenPort :: Int,
    apiServerDaemonize :: Bool,
    apiServerFetchPeriod :: Int,
    apiServerSecret :: String
    -- TODO: tls
  }
  deriving (Show)

apiServerOptions :: Parser APIServerOptions
apiServerOptions =
  APIServerOptions
    <$ enableAPIServer
    <*> strOption
      ( long "addr"
          <> help "API server address(default: 127.0.0.1)"
          <> value "127.0.0.1"
          <> metavar "ADDRESS"
      )
    <*> option
      auto
      ( long "port"
          <> help "API server port(default: 8778)"
          <> value 8778
          <> metavar "PORT"
      )
    <*> switch
      ( long "deamonize"
          <> short 'd'
          <> help "Run as a daemon"
      )
    <*> option
      auto
      ( long "fetch-period"
          <> help "When should the service fetch the new subscription data, in seconds.(default: 600)"
          <> value 600
      )
    <*> strOption
      ( long "secret"
          <> help "The secret. If not empty, access /?secret=<SECRET> to get the configuration."
          <> value ""
      )

data Todo
  = Out Output
  | Serve APIServerOptions
  deriving (Show)

todo :: Parser Todo
todo =
  (Out <$> output)
    <|> (Serve <$> apiServerOptions)

data Options = Options SubscriptionLink Todo deriving (Show)

options :: Parser Options
options = Options <$> sublink <*> todo

getOptions :: IO Options
getOptions = execParser p
  where
    p =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Convert just my socks subscription to clash configuration"
            <> header "jms2clash - jms -> clash subscription converter"
        )