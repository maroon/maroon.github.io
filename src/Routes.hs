{-# LANGUAGE OverloadedStrings #-}

module Routes
  ( dateRoute
  , indexRoute
  , tagsRoute
  ) where

import Hakyll
import System.FilePath ((</>), dropExtension)

dateRoute :: Routes
dateRoute = foldl1 composeRoutes routes
  where
    pattern = "/[0-9]{4}-[0-9]{2}-[0-9]{2}-"
    routes  =
      [ gsubRoute pattern (replaceAll "-" $ const "/")
      , indexRoute
      ]

indexRoute :: Routes
indexRoute = foldl1 composeRoutes routes
  where
    routes =
      [ customRoute ((</> "index") . dropExtension . toFilePath)
      , setExtension "html"
      ]

tagsRoute :: Routes
tagsRoute = foldl1 composeRoutes routes
  where
    routes =
      [ constRoute $ toFilePath "tags/"
      , indexRoute
      ]