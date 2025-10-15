---
title: "Static Site Generator with Makefile"
date: 2025-10-15
tags: ["makefile", "linux", "web"]
author: "Andy Sukowski-Bang"
description: "A minimal [`Makefile`](https://en.wikipedia.org/wiki/Make_(software))-based static site generator. Convert [Markdown](https://pandoc.org/MANUAL.html#pandocs-markdown) files to HTML with [Pandoc](https://pandoc.org/), copy other assets with [`rsync`](https://rsync.samba.org/), and preserve the directory structure."
---

For complex static sites like [andy.sb](/), I use [Hugo][hugo] with my [noJS theme][nojs],
but for simple sites, [the following `Makefile`](#makefile) using [Pandoc][pandoc] and [`rsync`][rsync] suffices.

## Expectation

A [Markdown][markdown] file `src/path/to/page.md` is converted to an HTML file `dst/path/to/page.html` via [Pandoc][pandoc],
while non-Markdown files are copied via [`rsync`][rsync].
The directory structure is preserved.

```
src/
├── about.md
├── blog
│   ├── image.png
│   └── post.md
└── style.css
```

The above `src/` directory results in the following `dst/` directory.
Note the conversion from `.md` to `.html`.

```
dst/
├── about.html
├── blog
│   ├── image.png
│   └── post.html
└── style.css
```

## Makefile

You can add [options][pandoc-options] like `--css=/style.css` to the Pandoc command.

```make
SRC_DIR = src
DST_DIR = dst

MD_FILES = $(shell find $(SRC_DIR) -type f -name '*.md')
HTML_FILES = $(MD_FILES:$(SRC_DIR)/%.md=$(DST_DIR)/%.html)

all: $(HTML_FILES) static

$(DST_DIR)/%.html: $(SRC_DIR)/%.md
	@mkdir -p $(@D)
	pandoc --standalone -o $@ $<

static:
	@mkdir -p $(DST_DIR)
	rsync -rl --exclude='*.md' $(SRC_DIR)/ $(DST_DIR)/

clean:
	rm -rf $(DST_DIR)

.PHONY: all static clean
```

[hugo]: https://gohugo.io/
[markdown]: https://pandoc.org/MANUAL.html#pandocs-markdown
[nojs]: https://git.andy.sb/nojs/
[pandoc]: https://pandoc.org/
[pandoc-options]: https://pandoc.org/MANUAL.html#options
[rsync]: https://rsync.samba.org/
