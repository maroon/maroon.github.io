--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Archive
import Config
import Contexts
import Data.Monoid ((<>))
import Feed
import Hakyll
import ScssCompiler
import System.FilePath ((</>), dropExtension)

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


-- Values ----------------------------------------------------------------------
postsPerPage :: Int
postsPerPage = 10


-- Routes ----------------------------------------------------------------------
dateRoute :: Routes
dateRoute =
  gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (replaceAll "-" $ const "/") `composeRoutes`
  indexRoute

indexRoute :: Routes
indexRoute =
  customRoute ((</> "index") . dropExtension . toFilePath) `composeRoutes`
  setExtension "html"
