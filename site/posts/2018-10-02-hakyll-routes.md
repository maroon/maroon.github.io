---
title: Hakyll Routes
date: 2018-10-02 12:35:52 -0500
tags: haskell, hakyll
---

During the move to Hakyll, I decided it would be best to reflect on the slugs used within the site. The idea would be to strip off any HTML matter from the slug, making the slug itself the star of the show. The end result would look something like `/posts/yyyy/mm/dd/post-name/` or `/page-name/`.

I would need a base route to begin this process; a way to transmogrify all pages so that they had an `index.html` ending. From there I could build up more complex routes that were specific to different domains. Hakyll made composing routes very simple by providing a function aptly named `composeRoutes`.

``` haskell
indexRoute :: Routes
indexRoute =
  customRoute ((</> "index") . dropExtension . toFilePath) `composeRoutes`
  setExtension "html"
```

This takes whatever the file path of the identifier is, drops its extension, and appends `/index.html` to it. This gave me exactly what I was looking for and is now the route through which all others will pass. I then moved on to the heart of the system: posts.

``` zsh
→ ls -1p posts/
2016-10-31-swift-package-manager-and-generics.md
2017-10-14-swift-reusable-codable-structures.md
2018-08-31-hakyll-sass-dependency.md
```

Posts within the site adhere to a very simple date-title format. `gsubRoute` can be used to match against this pattern. By feeding it the date pattern and a function that replaces the dashes with directory separators, the desired directory structure is produced.

``` haskell
dateRoute :: Routes
dateRoute =
  gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (replaceAll "-" $ const "/") `composeRoutes`
  indexRoute
```

This runs a regex over an identifier, replacing the post file format with an isomorphic directory structure. Note the composition with `indexRoute` above, which yields the final post slug: `/yyyy/mm/dd/post-name/index.html`.

``` haskell
match "posts/*" $ do
  route dateRoute
```

The last step is to add or update a `Rules` structure with the given route, and that's it!
