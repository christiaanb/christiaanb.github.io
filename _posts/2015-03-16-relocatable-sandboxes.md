---
layout: post
title:  "Relocatable Cabal sandboxes"
date:   2015-03-16 15:18:00
categories: blogging
tags: haskell cabal
comments: true
analytics: true
---

Cabal 1.22 has preliminary support for relocatable packages, which is actually undocumented because I'm a bad patch writer.

Regardless, this preliminary support for relocatable packages partially enables relocatable sandboxes. What's this partial part you might ask?
Well, only the `.cabal-sandbox` directory is relocatable, not the sandbox config file. So basically the part that takes the longest time to build is relocatable.
So, how do you go about the creating such a relocatable `.cabal-sandbox `directory? easy:

  * `cd <project_dir>`

  * `cabal sandbox init`

  * `cabal install --dependencies-only --enable-relocatable`

<br>

The created `.cabal-sandbox` directory is relocatable anywhere on the machine, and across machines if ghc is installed in the same directory.
Even dynamically linked libraries will work on Linux and OS X because the libraries will use relative RPATHs.

All that's left to having truly relocatable sandboxes, is to make the .config files also relocatable.
And I should write some documentation for the next Cabal release.
