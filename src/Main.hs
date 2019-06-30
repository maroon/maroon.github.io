module Main where

import Config (Config, loadConfiguration)
import Hakyll
import Rules (hakyllRules)

main :: IO ()
main = loadConfiguration >>= runHakyll

runHakyll :: Config -> IO ()
runHakyll yamlConfig = do
  let config = defaultConfiguration {
      providerDirectory = "site/"
    }
  hakyllWith config $ hakyllRules yamlConfig
