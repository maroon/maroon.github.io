module Feed
  ( feedConfig
  ) where

import Config
import Hakyll

feedConfig :: Config -> FeedConfiguration
feedConfig config = FeedConfiguration
  { feedTitle = title config
  , feedDescription = description config
  , feedRoot = baseUrl config
  , feedAuthorName = author . social $ config
  , feedAuthorEmail = email . social $ config
  }
