---
title: "Colimits of Enriched Categories"
date: 2026-10-11
tags: ["category-theory", "mathematics"]
author: "Andy Sukowski-Bang"
description: "I typeset a [poster](/posts/colimits-of-enriched-categories/poster.pdf) and [slides](/posts/colimits-of-enriched-categories/slides.pdf) on my paper [_Colimits of Enriched Categories_](https://arxiv.org/abs/2607.07780) for summer schools and conferences in [Göttingen](https://events.gwdg.de/event/1365/page/510-poster-session), [Kiel](https://www.math.uni-kiel.de/geometrie/de/essig/stratKiel2026/abstracts) and [Bonn](https://www.mpim-bonn.mpg.de/nps60). I used [Typst](https://typst.app/) with the packages [fletcher](https://github.com/fletcher-package/fletcher), [Polylux](https://polylux.dev/book/), [mannot](https://github.com/ryuryu-ymj/mannot) and [Showybox](https://github.com/Pablo-Gonzalez-Calderon/showybox-package)."
---

This supplementary material to my recent paper _Colimits of Enriched Categories_ (i.e. in [\(\mathcal{V}\text{-}\mathrm{Cat}\)][V-Cat]) at [arXiv:2607.07780][paper]
arose for poster sessions and lightning talks at [Göttingen][goettingen], [Kiel][kiel] and [Bonn][bonn].
I'm happy to answer questions at [uni@andy.sb](mailto:uni@andy.sb).

> We introduce categories of strings to give an explicit hom-object formula for [colimits][colimit] of [enriched categories][V-Cat].
> As corollaries, we recover the [necklace construction][necklace] by Dugger and Spivak and describe colimits in [\(\mathrm{Cat}\)][Cat] and of [Lawvere metric spaces][lawvere].

I also posted a self-contained version of the explicit formula for colimits in \(\mathcal{V}\text{-}\mathrm{Cat}\) [on MathOverflow][mo]
and a summary of Section 3 [on my blog][colimit-grothendieck].
I'm deviating from the paper's notation by omitting the bar on \(\mathcal{S}\) and \(\operatorname{ev}\) from Corollary 4.21.
In [the paper][paper] this bar-free notation is reserved for Definition 4.5.

## Poster

[This poster](poster.pdf) was typeset with [Typst][typst].
The diagrams were created with [fletcher][fletcher] and the colored boxes with [Showybox][showybox].

<div style="resize: vertical; overflow: hidden; height: 400px;">
  <embed type="application/pdf" src="poster.pdf" style="width: 100%; height: 100%;">
</div>

## Slides

[These slides](slides.pdf) were also typeset with [Typst][typst]
and the packages [Polylux][polylux] for slides, [fletcher][fletcher] for diagrams and [Mannot][mannot] for annotations.

<div style="resize: vertical; overflow: hidden; height: 400px;">
  <embed type="application/pdf" src="slides.pdf" style="width: 100%; height: 100%;">
</div>

[paper]: https://arxiv.org/abs/2607.07780
[goettingen]: https://events.gwdg.de/event/1365/page/510-poster-session
[kiel]: https://www.math.uni-kiel.de/geometrie/de/essig/stratKiel2026/abstracts
[bonn]: https://www.mpim-bonn.mpg.de/nps60
[mo]: https://mathoverflow.net/a/513164
[colimit-grothendieck]: https://andy.sb/posts/colimit-grothendieck/
[colimit]: https://ncatlab.org/nlab/show/colimit
[V-Cat]: https://ncatlab.org/nlab/show/category+of+V-enriched+categories
[necklace]: https://arxiv.org/abs/0910.0814
[Cat]: https://ncatlab.org/nlab/show/Cat
[lawvere]: https://ncatlab.org/nlab/show/metric+space#LawvereMetricSpace
[typst]: https://typst.app/
[fletcher]: https://github.com/fletcher-package/fletcher
[showybox]: https://github.com/Pablo-Gonzalez-Calderon/showybox-package
[polylux]: https://polylux.dev/book/
[mannot]: https://github.com/ryuryu-ymj/mannot
