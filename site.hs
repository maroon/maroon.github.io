--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import Data.Monoid ((<>))
import Data.Yaml (FromJSON (..), Value (..), (.:), (.:?))
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Hakyll
import System.FilePath ((</>), dropExtension, takeDirectory,
                        splitFileName, addTrailingPathSeparator)

--------------------------------------------------------------------------------
main :: IO ()
main = loadConfiguration >>= runHakyll

runHakyll :: Config -> IO ()
runHakyll config = hakyll $ do
  let postCtx = postContext config
  let metaCtx = metaContext config

  match "assets/**" $ do
    route idRoute
    compile copyFileCompiler

  match "scss/**.scss" $ do
    route idRoute
    compile getResourceString

  scssDependency <- makePatternDependency "scss/**.scss"
  rulesExtraDependencies [scssDependency] $ do
    create ["assets/main.css"] $ do
      route idRoute
      compile $
        loadBody "scss/novella.scss"
          >>= makeItem
          >>= scssCompiler

  match "about.md" $ do
    route indexRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post/body.html" postCtx
      >>= loadAndApplyTemplate "templates/site/body.html" postCtx
      >>= relativizeUrls

  match "posts/*" $ do
    route dateRoute
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post/body.html" postCtx
      >>= loadAndApplyTemplate "templates/site/body.html" postCtx
      >>= relativizeUrls

  page <- buildPaginateWith archiveGroup "posts/*" archiveId
  paginateRules page $ \pageNum pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let paginateCtx = paginateContext page pageNum
          ctx =
            constField "title" "Archives" <>
            listField "posts" postCtx (return posts) <>
            paginateCtx <>
            postCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive/body.html" ctx
        >>= loadAndApplyTemplate "templates/site/body.html" ctx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts) <>
            constField "title" "Home" <>
            boolField "show_archive" (const $ length posts > postsPerPage) <>
            metaCtx

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/site/body.html" indexCtx
        >>= relativizeUrls

  match "templates/**" $ compile templateBodyCompiler

  create ["feed.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx =
            bodyField "description" <>
            postCtx

      posts <- fmap (take postsPerPage) . recentFirst
        =<< loadAllSnapshots "posts/*" "content"

      renderAtom (feedConfig config) feedCtx posts


-- YAML ------------------------------------------------------------------------
data Config = Config
  { title :: String
  , description :: String
  , baseUrl :: String
  , display :: Display
  , social :: Social
  } deriving (Eq, Show)

data Display = Display
  { showFeed :: Bool
  , showEmail :: Bool
  , showGithub :: Bool
  , showTwitter :: Bool
  } deriving (Eq, Show)

data Social = Social
  { author :: String
  , email :: String
  , github :: Maybe String
  , twitter :: Maybe String
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .: "title"
    <*> v .: "description"
    <*> v .: "base_url"
    <*> v .: "display"
    <*> v .: "social"

instance FromJSON Display where
  parseJSON (Object v) = Display
    <$> v .: "feed"
    <*> v .: "email"
    <*> v .: "github"
    <*> v .: "twitter"

instance FromJSON Social where
  parseJSON (Object v) = Social
    <$> v .:  "author"
    <*> v .:  "email"
    <*> v .:? "github"
    <*> v .:? "twitter"

loadConfiguration :: IO Config
loadConfiguration = loadYamlSettings ["config.yaml"] [] useEnv


-- Values ----------------------------------------------------------------------
postsPerPage :: Int
postsPerPage = 10


-- Feeds -----------------------------------------------------------------------
feedConfig :: Config -> FeedConfiguration
feedConfig config = FeedConfiguration
  { feedTitle = title config
  , feedDescription = description config
  , feedRoot = baseUrl config
  , feedAuthorName = author . social $ config
  , feedAuthorEmail = email . social $ config
  }


-- Routes ----------------------------------------------------------------------
dateRoute :: Routes
dateRoute =
  gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (replaceAll "-" $ const "/") `composeRoutes`
  indexRoute

indexRoute :: Routes
indexRoute =
  customRoute ((</> "index") . dropExtension . toFilePath) `composeRoutes`
  setExtension "html"


-- Pagination  -----------------------------------------------------------------
archiveGroup :: MonadMetadata m => [Identifier] -> m [[Identifier]]
archiveGroup = liftM (paginateEvery postsPerPage . drop postsPerPage) . sortRecentFirst

archiveId :: PageNumber -> Identifier
archiveId pageNum = fromFilePath $ "archive/page/" ++ (show pageNum) ++ "/index.html"


-- Compilers -------------------------------------------------------------------
scssCompiler :: Item String -> Compiler (Item String)
scssCompiler = withItemBody (unixFilter "sass" [ "-s"
                                               , "--scss"
                                               , "--style", "compressed"
                                               , "--load-path", "scss"
                                               ])


-- Contexts --------------------------------------------------------------------
maybeField :: String -> Maybe String -> Context a
maybeField _ Nothing    = mempty
maybeField key (Just v) = constField key v

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
