--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
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

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts) `mappend`
            constField "title" "Archives"            `mappend`
            metaCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/site/body.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts) `mappend`
            constField "title" "Home"                `mappend`
            metaCtx

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/site/body.html" indexCtx
        >>= relativizeUrls

  match "templates/**" $ compile templateBodyCompiler


-- Functions -------------------------------------------------------------------
dateRoute :: Routes
dateRoute =
  gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (replaceAll "-" $ const "/") `composeRoutes`
  customRoute (dropExtension . toFilePath) `composeRoutes`
  setExtension "html"


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
  constField "site_title" "Maroon" `mappend`
  constField "name" "Ryan Maroon"  `mappend`
  constField "github" "maroon"     `mappend`
  defaultContext

postCtx :: Context String
postCtx =
  boolField "is_post" (const True) `mappend`
  dateField "date" "%b %e, %Y"     `mappend`
  metaCtx
