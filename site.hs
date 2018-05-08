--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (liftM)
import Data.Monoid ((<>))
import Hakyll
import System.FilePath (dropExtension)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "assets/**" $ do
    route idRoute
    compile copyFileCompiler

  match "scss/*.scss" $ do
    route $ constRoute "assets/main.css"
    compile compressScssCompiler

  match "about.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post/body.html" postCtx
      >>= loadAndApplyTemplate "templates/site/body.html" postCtx
      >>= relativizeUrls

  match "posts/*" $ do
    route dateRoute
    compile $ pandocCompiler
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

-- Values ----------------------------------------------------------------------
postsPerPage :: Int
postsPerPage = 10


-- Routes ----------------------------------------------------------------------
dateRoute :: Routes
dateRoute =
  gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (replaceAll "-" $ const "/") `composeRoutes`
  customRoute (dropExtension . toFilePath) `composeRoutes`
  setExtension "html"


-- Pagination  -----------------------------------------------------------------
archiveGroup :: MonadMetadata m => [Identifier] -> m [[Identifier]]
archiveGroup = liftM (paginateEvery postsPerPage . drop postsPerPage) . sortRecentFirst

archiveId :: PageNumber -> Identifier
archiveId pageNum = fromFilePath $ "archive/page/" ++ (show pageNum) ++ "/index.html"


-- Compilers -------------------------------------------------------------------
compressScssCompiler :: Compiler (Item String)
compressScssCompiler =
  getResourceString
    >>= withItemBody (unixFilter "sass" [ "-s"
                                        , "--scss"
                                        , "--style", "compressed"
                                        , "--load-path", "scss"
                                        ])


-- Contexts --------------------------------------------------------------------
metaCtx :: Context String
metaCtx =
  constField "site_title" "Maroon" <>
  constField "name" "Ryan Maroon" <>
  constField "github" "maroon" <>
  defaultContext

postCtx :: Context String
postCtx =
  boolField "is_post" (const True) <>
  dateField "date" "%b %e, %Y" <>
  metaCtx
