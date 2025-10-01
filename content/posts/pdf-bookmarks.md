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

I wanted a simpler syntax for specifying the outline.

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

import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Parsec
import Text.Parsec.String (Parser)

data Bookmark = Bookmark
    { title :: String,
      level :: Int,
      page  :: Int }

tree :: Parser [Bookmark]
tree = many line <* skipMany endOfLine <* eof
  where
    line =
      (\tabs page title -> Bookmark title (length tabs + 1) (read page))
        <$> many (char '\t')
        <*> many1 digit <* space
        <*> manyTill anyChar (() <$ endOfLine <|> eof)

pdftk :: [Bookmark] -> String
pdftk = unlines . concatMap render
  where
    render (Bookmark title level page) =
      [ "BookmarkBegin"
      , "BookmarkTitle: "      ++ title
      , "BookmarkLevel: "      ++ show level
      , "BookmarkPageNumber: " ++ show page
      , "" ]

main :: IO ()
main = getArgs >>= \case
    [f] -> readFile f >>= either print (putStr . pdftk) . parse tree f
    _   -> getProgName >>= \pn -> die $ "Usage: " ++ pn ++ " <file>"
```

I admit that using Haskell is a bit overkill and [AWK][4] would suffice.

[1]: https://www.pdflabs.com/tools/pdftk-server/
[2]: https://www.haskell.org/
[3]: https://hackage.haskell.org/package/parsec
[4]: https://en.wikipedia.org/wiki/AWK
