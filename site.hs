--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (empty)
import Control.Monad (forM)
import Data.Bool (bool)
import Data.List (groupBy)
import Data.Monoid ((<>))
import Data.Time.Calendar (toGregorian)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Time.LocalTime (localDay, utcToLocalTime, utc)
import Data.Yaml (FromJSON (..), Value (..), (.:), (.:?))
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Hakyll
import System.FilePath ((</>), dropExtension, takeDirectory,
                        splitFileName, addTrailingPathSeparator)

import qualified Data.Map as M
import qualified Data.Set as S

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

  page <- buildArchivePaginateWith archiveGroup "posts/*" archiveId
  paginateRules page $ \pageNum pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let paginateCtx = archiveContext page pageNum
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
      archiveYear <- yearForIdentifier (itemIdentifier . head $ posts)
      let archivePage = show . archiveId $ archiveYear
      let indexCtx =
            listField "posts" postCtx (return posts) <>
            constField "title" "Home" <>
            boolField "show_archive" (const $ length posts > postsPerPage) <>
            constField "archive_page" archivePage <>
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
yearForIdentifier :: (MonadMetadata m, Num a) => Identifier -> m a
yearForIdentifier i = do
  time <- getItemUTC defaultTimeLocale i
  let (year, _, _) = toGregorian . localDay $ utcToLocalTime utc time
  return $ fromIntegral year

archiveGroup :: MonadMetadata m => [Identifier] -> m [[Identifier]]
archiveGroup ids = do
  sorted <- mapM yearMap =<< sortRecentFirst ids
  return $ (fmap snd) <$> groupBy (\a b -> fst a == fst b) sorted
  where
    yearMap :: MonadMetadata m => Identifier -> m (Integer, Identifier)
    yearMap i = yearForIdentifier i >>= \year -> return (year, i)

archiveId :: PageNumber -> Identifier
archiveId pageNum = fromFilePath $ "archive/year/" ++ (show pageNum) ++ "/index.html"

archivePages :: MonadMetadata m => [Identifier] -> m PageNumber
archivePages = yearForIdentifier . head

buildArchivePaginateWith :: MonadMetadata m
                         => ([Identifier] -> m [[Identifier]])
                         -> Pattern
                         -> (PageNumber -> Identifier)
                         -> m Paginate
buildArchivePaginateWith grouper pattern makeId = do
  ids      <- getMatches pattern
  idGroups <- grouper ids
  years    <- mapM archivePages idGroups
  let idsSet = S.fromList ids
  return Paginate
    { paginateMap        = M.fromList $ zip years idGroups
    , paginateMakeId     = makeId
    , paginateDependency = PatternDependency pattern idsSet
    }


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

archiveContext :: Paginate -> PageNumber -> Context a
archiveContext pag currentPage = mconcat
  [ field "firstPageNum"    $ \_ -> otherPage lastPage      >>= num
  , field "firstPageUrl"    $ \_ -> otherPage lastPage      >>= url
  , field "previousPageNum" $ \_ -> otherPage previousPage  >>= num
  , field "previousPageUrl" $ \_ -> otherPage previousPage  >>= url
  , field "nextPageNum"     $ \_ -> otherPage nextPage      >>= num
  , field "nextPageUrl"     $ \_ -> otherPage nextPage      >>= url
  , field "lastPageNum"     $ \_ -> otherPage firstPage     >>= num
  , field "lastPageUrl"     $ \_ -> otherPage firstPage     >>= url
  , field "currentPageNum"  $ \i -> thisPage i              >>= num
  , field "currentPageUrl"  $ \i -> thisPage i              >>= url
  , constField "numPages"   $ show lastPage
  , Context $ \k _ i ->
      case k of
        "allPages" -> do
          let ctx =
                field "isCurrent" (\n -> bool empty (return "true") $ fst (itemBody n) == currentPage) <>
                field "num" (num . itemBody) <>
                field "url" (url . itemBody)

          list <- forM [1..lastPage] $
            \n -> bool (otherPage n) (thisPage i) (n == currentPage)
          items <- mapM makeItem list
          return $ ListField ctx items
        _          -> do
          empty
  ]
  where
    firstPage :: PageNumber
    firstPage = head . M.keys $ paginateMap pag

    lastPage :: PageNumber
    lastPage = last . M.keys $ paginateMap pag

    previousPage :: PageNumber
    previousPage = do
      let pages = M.keys . snd $ M.split currentPage (paginateMap pag)
      bool (head pages) 0 $ null pages

    nextPage :: PageNumber
    nextPage = do
      let pages = M.keys . fst $ M.split currentPage (paginateMap pag)
      bool (last pages) 0 $ null pages

    thisPage :: Item a -> Compiler (Int, Identifier)
    thisPage i = return (currentPage, itemIdentifier i)

    otherPage :: PageNumber -> Compiler (Int, Identifier)
    otherPage n
      | n == currentPage = fail $ "This is the current page: " ++ show n
      | otherwise        =
        case M.member n (paginateMap pag) of
          True  -> return (n, paginateMakeId pag n)
          False -> fail $ "No such page: " ++ show n

    num :: (Int, Identifier) -> Compiler String
    num = return . show . fst

    url :: (Int, Identifier) -> Compiler String
    url (n, i) = getRoute i >>= \mbR -> case mbR of
      Just r  -> return $ toUrl r
      Nothing -> fail $ "No URL for page: " ++ show n
