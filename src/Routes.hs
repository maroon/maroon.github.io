module Routes
  ( dateRoute
  , indexRoute
  ) where

import Hakyll
import System.FilePath ((</>), dropExtension)

dateRoute :: Routes
dateRoute =
  gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (replaceAll "-" $ const "/") `composeRoutes`
  indexRoute

indexRoute :: Routes
indexRoute =
  customRoute ((</> "index") . dropExtension . toFilePath) `composeRoutes`
  setExtension "html"
