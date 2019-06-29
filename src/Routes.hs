module Routes
  ( dateRoute
  , indexRoute
  , siteRoute
  ) where

import Hakyll
import System.FilePath ((</>), dropExtension)

dateRoute :: Routes
dateRoute =
  siteRoute `composeRoutes`
  gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (replaceAll "-" $ const "/") `composeRoutes`
  indexRoute

indexRoute :: Routes
indexRoute =
  siteRoute `composeRoutes`
  customRoute ((</> "index") . dropExtension . toFilePath) `composeRoutes`
  setExtension "html"

siteRoute :: Routes
siteRoute =
  gsubRoute "site/" $ const ""
