---
title: "Colimit functoriality on Grothendieck construction"
date: 2026-03-17
tags: ["category-theory", "mathematics"]
author: "Andy Sukowski-Bang"
description: "Taking colimits is functorial on diagrams with varying index categories via the Grothendieck construction. A summary of Section 3 of _Colimits of Enriched Categories_, see [arXiv:2607.07780](https://arxiv.org/abs/2607.07780)."
---

This post summarizes Section 3 of _Colimits of Enriched Categories_, see [arXiv:2607.07780][arxiv].

Can taking [colimits][colimit] be regarded as a functor?
Let \(\mathcal{C}\) be [cocomplete][cocomplete] and \(\mathcal{I}\) a small category.
Then \(\operatorname{colim} \colon [\mathcal{I},\mathcal{C}] \to \mathcal{C}\) is functorial on the [functor category][functor-category].
More generally, the domain of \(\operatorname{colim}\) extends to the Grothendieck construction \(\int [-,\mathcal{C}]\),
which keeps track of index changes \(\mathcal{I} \to \mathcal{J}\).

## Grothendieck construction

Let \(F \colon \mathcal{D}^\mathrm{op} \to \mathrm{Cat}\) be a contravariant functor.
The **Grothendieck construction** \(\int F\) is the category where
- objects are pairs \((d,e)\) of \(d \in \mathcal{D}\) and \(e \in F(d)\),
- morphisms \((d,e) \to (d',e')\) are given by pairs \((f \colon d \to d', e \to (F f)(e'))\),
- composition is defined as \((g,\psi) \circ (f,\varphi) \coloneqq (g f, (F f)(\psi) \circ \varphi)\).

## Functoriality of colimit

Let \(\mathcal{C}\) be [cocomplete][cocomplete].
A choice of [colimit][colimit] per diagram defines a functor
\[
  \begin{aligned}
    \operatorname{colim} \colon \int [-,\mathcal{C}] &\to \mathcal{C}, \\
    (\mathcal{I},F) &\mapsto \operatorname{colim}(F).
  \end{aligned}
\]
A morphism \((\mathcal{I},F) \to (\mathcal{J},G)\) consists of a functor \(H \colon \mathcal{I} \to \mathcal{J}\)
and a natural transformation \(\eta \colon F \Rightarrow [H,\mathcal{C}](G) \coloneqq G \circ H\).
[Whiskering][whiskering] the colimiting [cocone][cocone] \(\iota \colon G \Rightarrow \Delta_\mathcal{J} \operatorname{colim}(G)\) with \(H\),
and precomposing with \(\eta\), we obtain the cocone
\[
    F \xRightarrow{\eta} G \circ H
    \xRightarrow{\iota H} (\Delta_\mathcal{J} \operatorname{colim}(G)) \circ H
    = \Delta_\mathcal{I} \operatorname{colim}(G).
\]
The universal property of \(\operatorname{colim}(F)\) implies a unique cocone morphism
\(\operatorname{colim}(H,\eta) \colon \operatorname{colim}(F) \to \operatorname{colim}(G)\).
I ran out of letters, so I will leave the preservation of composition and identity as an exercise in bookkeeping.

## Strong monoidal functoriality

A [monoidal category][monoidal-category] \((\mathcal{C},\otimes,\mathbf{1})\) canonically endows \(\int [-,\mathcal{C}]\) with a monoidal structure
given by the unit \((\{*\}, * \mapsto \mathbf{1})\) on the [terminal category][terminal-category] and the tensor product
\[
  (\mathcal{I},F) \boxtimes (\mathcal{J},G)
  \coloneqq (\mathcal{I} \times \mathcal{J}, \otimes \circ (F,G)).
\]
Let \(\mathcal{C}\) be cocomplete and let \(\otimes\) be [cocontinuous][cocontinuous] in each argument.
Then \(\operatorname{colim}\) is a [strong monoidal functor][monoidal-functor]
with \(\mathbf{1} \to \operatorname{colim}(* \mapsto \mathbf{1})\) given by the colimiting cocone and
\(
    \operatorname{colim}(F) \otimes \operatorname{colim}(G)
    \cong \operatorname{colim}(F \boxtimes G)
\)
by cocontinuity and [Fubini for colimits][fubini].

[cocomplete]: https://ncatlab.org/nlab/show/cocomplete+category
[cocone]: https://ncatlab.org/nlab/show/cocone
[cocontinuous]: https://ncatlab.org/nlab/show/cocontinuous+functor
[colimit]: https://en.wikipedia.org/wiki/Limit_(category_theory)#Colimits
[functor-category]: https://en.wikipedia.org/wiki/Functor_category
[monoidal-category]: https://ncatlab.org/nlab/show/monoidal+category
[monoidal-functor]: https://ncatlab.org/nlab/show/monoidal+functor
[terminal-category]: https://ncatlab.org/nlab/show/terminal+category
[whiskering]: https://ncatlab.org/nlab/show/whiskering
[fubini]: https://stacks.math.columbia.edu/tag/002M
[arxiv]: https://arxiv.org/abs/2607.07780
