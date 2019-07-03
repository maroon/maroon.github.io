{-# LANGUAGE OverloadedStrings #-}

module Rules
  ( hakyllRules
  ) where

import Archive
  ( buildArchivePaginateWith
  , archiveContext
  , archiveGroup
  , archiveId
  , yearForIdentifier
  )
import Config (Config)
import Contexts
  ( metaContext
  , postContext
  , tagsContext
  )
import Data.Monoid ((<>))
import Feed (feedConfig)
import Hakyll
import Routes (dateRoute, indexRoute, tagsRoute)
import ScssCompiler (scssCompiler)
import qualified Config as C (display, postsPerPage)

hakyllRules :: Config -> Rules ()
hakyllRules config = do
  let postCtx = postContext config
  let metaCtx = metaContext config
  let postsPerPage = C.postsPerPage . C.display $ config

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

  tags <- buildTags "posts/*" (fromCapture "tags/*/index.html")
  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx =
            constField "title" tag <>
            listField "posts" postCtx (return posts) <>
            postCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag/body.html" ctx
        >>= loadAndApplyTemplate "templates/site/body.html" ctx
        >>= relativizeUrls

  match "tags.html" $ do
    route tagsRoute
    compile $ do
      tags' <- loadAll "tags/*/index.html"
      let tagCtx =
            constField "title" "Tags" <>
            listField "tags" tagsContext (return tags') <>
            postCtx

      getResourceBody
        >>= applyAsTemplate tagCtx
        >>= loadAndApplyTemplate "templates/site/body.html" tagCtx
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
      let showArchive = (const $ length posts > postsPerPage)
      let archivePage = show . archiveId $ archiveYear
      let indexCtx =
            listField "posts" postCtx (return posts) <>
            constField "title" "Home" <>
            boolField "show_archive" showArchive <>
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
