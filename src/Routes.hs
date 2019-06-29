module Routes
  ( dateRoute
  , indexRoute
  , siteRoute
  ) where

import Hakyll
import System.FilePath ((</>), dropExtension)

dateRoute :: Routes
dateRoute = foldl1 composeRoutes routes
  where
    pattern = "/[0-9]{4}-[0-9]{2}-[0-9]{2}-"
    routes  =
      [ siteRoute
      , gsubRoute pattern (replaceAll "-" $ const "/")
      , indexRoute
      ]

indexRoute :: Routes
indexRoute = foldl1 composeRoutes routes
  where
    routes =
      [ siteRoute
      , customRoute ((</> "index") . dropExtension . toFilePath)
      , setExtension "html"
      ]

siteRoute :: Routes
siteRoute = gsubRoute "site/" $ const ""
