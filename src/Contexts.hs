module Contexts
  ( metaContext
  , postContext
  , tagsContext
  ) where

import Config
import Hakyll
import System.FilePath
  ( takeDirectory
  , splitFileName
  , addTrailingPathSeparator
  , takeBaseName
  )

maybeField :: String -> Maybe String -> Context a
maybeField _ Nothing    = mempty
maybeField key (Just v) = constField key v

tagField :: String -> Context a
tagField = mapContext (takeBaseName . takeDirectory) . pathField

formatPostUrlContext :: Context a
formatPostUrlContext = mapContext format (urlField "url")
  where
    format url =
      case splitFileName url of
        (p, "index.html") -> addTrailingPathSeparator . takeDirectory $ p
        _                 -> url

metaContext :: Config -> Context String
metaContext config =
  constField "site_title" (title config) <>
  constField "site_desc" (description config) <>
  constField "name" (author . social $ config) <>
  constField "email" (email . social $ config) <>
  maybeField "github" (github . social $ config) <>
  maybeField "twitter" (twitter . social $ config) <>
  boolField "show_feed" (const . showFeed . display $ config) <>
  boolField "show_email" (const . showEmail . display $ config) <>
  boolField "show_github" (const . showGithub . display $ config) <>
  boolField "show_twitter" (const . showTwitter . display $ config) <>
  defaultContext

postContext :: Config -> Context String
postContext config =
  boolField "is_post" (const True) <>
  dateField "date" "%b %e, %Y" <>
  formatPostUrlContext <>
  metaContext config

tagsContext :: Context String
tagsContext = tagField "tag" <>
              defaultContext
