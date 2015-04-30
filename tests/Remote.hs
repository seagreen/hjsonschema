module Main where

import           Control.Applicative
import           Control.Concurrent.Async       (withAsync)
import           Data.List                      (isSuffixOf)
import           Lib
import           Network.Wai.Application.Static (defaultFileServerSettings,
                                                 staticApp)
import           Network.Wai.Handler.Warp       (run)
import           System.Directory               (getDirectoryContents)
import           Test.Framework

main :: IO ()
main =
  withAsync (run 1234 $ staticApp $ defaultFileServerSettings "JSON-Schema-Test-Suite/remotes") $ \_ -> do
    filenames <- filter (not . isLocal) . filter (".json" `isSuffixOf`)
                 <$> getDirectoryContents dir
    ts <- readSchemaTests dir filenames
    defaultMain (toTest <$> ts)

  where
    dir :: String
    dir = "JSON-Schema-Test-Suite/tests/draft4"
