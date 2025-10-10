---
title: "PDF Bookmarks with PDFtk and Parsec"
date: 2025-10-01
tags: ["haskell", "linux", "pdf"]
author: "Andy Sukowski-Bang"
description: "Use Haskell and [Parsec](https://hackage.haskell.org/package/parsec) to transpile a simple outline format into [PDFtk](https://www.pdflabs.com/tools/pdftk-server/)’s verbose bookmark syntax."
---

Some professors only upload hand-written notes, which are hard to navigate, so I add PDF bookmarks using [PDFtk][1].

```sh
pdftk in.pdf update_info in.info output out.pdf
```

## Bookmark Syntax

The problem is that the syntax for bookmarks in `in.info` is very verbose.

<pre style="height: 20rem" tabindex="0">
<code>BookmarkBegin
BookmarkTitle: Chapter 1
BookmarkLevel: 1
BookmarkPageNumber: 1

BookmarkBegin
BookmarkTitle: Section 1.1
BookmarkLevel: 2
BookmarkPageNumber: 6

BookmarkBegin
BookmarkTitle: Subsection 1.1.1
BookmarkLevel: 3
BookmarkPageNumber: 13

BookmarkBegin
BookmarkTitle: Subsection 1.1.2
BookmarkLevel: 3
BookmarkPageNumber: 18

BookmarkBegin
BookmarkTitle: Section 1.2
BookmarkLevel: 2
BookmarkPageNumber: 27

BookmarkBegin
BookmarkTitle: Subsection 1.2.1
BookmarkLevel: 3
BookmarkPageNumber: 31</code>
</pre>

I wanted a simpler syntax for specifying the outline (indent with tabs).

```
1 Chapter 1
	6 Section 1.1
		13 Subsection 1.1.1
		18 Subsection 1.1.2
	27 Section 1.2
		31 Subsection 1.2.1
```

## Haskell Transpiler

I wrote the following [Haskell][2] program, which parses my simple syntax using [Parsec][3] and converts it to the verbose PDFtk syntax.

```hs
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (intercalate)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

data Bookmark = Bookmark
  Int    -- ^ Level
  Int    -- ^ Page
  String -- ^ Title

tree :: Parser [Bookmark]
tree = bookmark `sepEndBy` endOfLine <* eof
  where
    bookmark = Bookmark <$> level <*> page <*> title
    level    = (+1) . length <$> many tab
    page     = read <$> many1 digit <* space
    title    = many (noneOf "\n")

pdftk :: [Bookmark] -> String
pdftk = intercalate "\n" . fmap render
  where
    render (Bookmark level page title) = unlines
      [ "BookmarkBegin"
      , "BookmarkTitle: "      ++ title
      , "BookmarkLevel: "      ++ show level
      , "BookmarkPageNumber: " ++ show page ]

main :: IO ()
main = getArgs >>= \case
  [f] -> parseFromFile tree f >>= either print (putStr . pdftk)
  _   -> getProgName >>= die . ("Usage: " ++) . (++ " <file>")
```

I admit that using Haskell is a bit overkill and [AWK][4] would suffice.

[1]: https://www.pdflabs.com/tools/pdftk-server/
[2]: https://www.haskell.org/
[3]: https://hackage.haskell.org/package/parsec
[4]: https://en.wikipedia.org/wiki/AWK
