---
title: "Hakyll Sass Dependency"
date: 2018-08-31 08:06:26 -0500
tags: [haskell, hakyll, sass, novella]
---

One of the very first adventures I had in Hakyll was coming up with a way to get the Sass compiler working with it. Some time ago I had published a [theme for Jekyll](https://github.com/maroon/novella), which I had decided I wanted to keep around for this blog. I simply created a `scss` directory which housed the theme's files.

``` zsh
→ ls -1p scss/
novella/
novella.scss
```

The solution didn't require much knowledge of the inner workings of Hakyll. Rather, it was more about knowing how to manipulate the arguments for `sass`.

``` haskell
match "scss/*.scss" $ do
  route $ constRoute "assets/main.css"
  compile compressScssCompiler

compressScssCompiler :: Compiler (Item String)
compressScssCompiler = withItemBody $ unixFilter "sass" arguments
  where
    arguments = [ "-s"
                , "--scss"
                , "--style", "compressed"
                , "--load-path", "scss"
                ]
```

Here I tell it to match any SCSS files within the `scss` directory, in this case it is only the `novella.scss` file, and that in turn is passed along to `sass`. Once `sass` is done doing its business, Hakyll then outputs the compiled data into `assets/main.css`.

It worked wonderfully, so I set about tackling more interesting problems with Hakyll. As work on the blog came to a close, I found myself wanting to tweak some designs within the theme. It felt as though it had accrued some cruft and I didn't care for some of the previous aesthetic choices I had made.

While making alterations I discovered that `site watch` wasn't observing the changes being made to the theme's imported files. The previous solution works so long as you make no changes to any imported SCSS files and your directory structure looks a little something like what I had.

Hakyll was only being told to watch `novella.scss`, and not the files contained within the `novella` subdirectory. These conditions made it remarkably unpleasant to work with while frequently iterating and tweaking the design. The seemingly obvious solution would be to use a glob that permits recursion and matching of the inner style sheets.

``` haskell
match "scss/**.scss" $ do
  route $ constRoute "assets/main.css"
  compile compressScssCompiler

compressScssCompiler :: Compiler (Item String)
compressScssCompiler = withItemBody $ unixFilter "sass" arguments
  where
    arguments = [ "-s"
                , "--scss"
                , "--style", "compressed"
                ]
```

This should work, only so long as there are no file dependencies. In the case of the Novella theme, there are many dependencies that are tied together by a single file.

After many failed attempts to skirt the issue, I discovered that Hakyll provides a marvellous solution to this very conundrum: `rulesExtraDependencies`. It ensures that any changes the compiler makes to files matching the specified dependencies are then processed with a follow-up build step. `makePatternDependency` is responsible for generating a `Dependency` structure from a given `Pattern`.

``` haskell
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

scssCompiler :: Item String -> Compiler (Item String)
scssCompiler = withItemBody $ unixFilter "sass" arguments
  where
    arguments = [ "-s"
                , "--scss"
                , "--style", "compressed"
                , "--load-path", "scss"
                ]
```

Now the matcher contains the recursive glob, which is also reflected in the `makePatternDependency` call. The compile step changes to retrieve the resource's contents rather than sending it to `sass`. This change allows the `novella.scss` contents to be recalled in the dependency step while skipping over any `sass` compilations for the imported SCSS files. The dependency step, as just mentioned, recalls the contents of `novella.scss` and passes it along to `sass`. Hakyll then takes the output and writes it to `assets/main.css`.

Voilà! Hakyll will now update the CSS file whenever the any of the SCSS files are modified.
