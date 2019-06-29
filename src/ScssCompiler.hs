module ScssCompiler
  ( scssCompiler
  ) where

import Hakyll

scssCompiler :: Item String -> Compiler (Item String)
scssCompiler = withItemBody $ unixFilter "sass" arguments
  where
    arguments = [ "-s"
                , "--scss"
                , "--style", "compressed"
                , "--load-path", "site/scss"
                ]

