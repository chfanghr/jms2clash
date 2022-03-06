module Main where

import APIServer ( startAPIServer, periodicallyFetchWorker )
import Control.Concurrent ( forkIO )
import GHC.Conc (newTVarIO)
import Options
    ( APIServerOptions(APIServerOptions),
      SubscriptionLink(SubscriptionLink),
      Output(..),
      Options(..),
      Todo(Serve, Out),
      getOptions )
import Subscription (fetchAndConvertSubscription)
import System.Posix.Daemonize ( daemonize )

execute :: Options -> IO ()
execute (Options (SubscriptionLink sub) (Out out)) =
  executeOut out sub
execute
  ( Options
      (SubscriptionLink sub)
      (Serve (APIServerOptions host port shouldDeamonize period secret))
    ) = do
    yaml <- fetchAndConvertSubscription sub
    confVar <- newTVarIO yaml

    let mainRoutine =
          ( do
              forkIO $ periodicallyFetchWorker confVar period sub
              startAPIServer confVar host port secret
          )

    if shouldDeamonize
      then daemonize mainRoutine
      else mainRoutine

executeOut :: Output -> String -> IO ()
executeOut out subUrl = do
  yaml <- fetchAndConvertSubscription subUrl
  case out of
    FileOutput s -> writeFile s yaml
    StdOutput -> putStrLn yaml

main :: IO ()
main = getOptions >>= execute