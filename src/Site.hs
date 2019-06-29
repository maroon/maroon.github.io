{-# LANGUAGE OverloadedStrings #-}

import Archive
import Contexts
import Config (Config, loadConfiguration)
import Data.Monoid ((<>))
import Feed
import Hakyll
import Routes
import ScssCompiler
import qualified Config as C (display, postsPerPage)

main :: IO ()
main = loadConfiguration >>= runHakyll

runHakyll :: Config -> IO ()
runHakyll config = hakyll $ do
  let postCtx = postContext config
  let metaCtx = metaContext config
  let postsPerPage = C.postsPerPage . C.display $ config

  match "site/assets/**" $ do
    route idRoute
    compile copyFileCompiler

  match "site/scss/**.scss" $ do
    compile getResourceString

  scssDependency <- makePatternDependency "site/scss/**.scss"
  rulesExtraDependencies [scssDependency] $ do
    create ["assets/main.css"] $ do
      route idRoute
      compile $
        loadBody "site/scss/novella.scss"
          >>= makeItem
          >>= scssCompiler

  match "site/about.md" $ do
    route indexRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "site/templates/post/body.html" postCtx
      >>= loadAndApplyTemplate "site/templates/site/body.html" postCtx
      >>= relativizeUrls

  match "site/posts/*" $ do
    route dateRoute
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "site/templates/post/body.html" postCtx
      >>= loadAndApplyTemplate "site/templates/site/body.html" postCtx
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
        >>= loadAndApplyTemplate "site/templates/archive/body.html" ctx
        >>= loadAndApplyTemplate "site/templates/site/body.html" ctx
        >>= relativizeUrls

  match "site/index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "site/posts/*"
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
        >>= loadAndApplyTemplate "site/templates/site/body.html" indexCtx
        >>= relativizeUrls

  match "site/templates/**" $ compile templateBodyCompiler

  create ["site/feed.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx =
            bodyField "description" <>
            postCtx

      posts <- fmap (take postsPerPage) . recentFirst
        =<< loadAllSnapshots "site/posts/*" "content"

      renderAtom (feedConfig config) feedCtx posts
