---
title: "Colimit functoriality on Grothendieck construction"
date: 2026-03-17
tags: ["category-theory", "mathematics"]
author: "Andy Sukowski-Bang"
description: "Taking colimits is functorial on diagrams with varying index categories via the Grothendieck construction."
---

Can taking [colimits][colimit] be regarded as a functor?
Let \(\mathcal{C}\) be [cocomplete][cocomplete] and \(\mathcal{I}\) a small category.
Then \(\operatorname{colim} \colon [\mathcal{I},\mathcal{C}] \to \mathcal{C}\) is a functor on the [functor category][functor-category].
More generally, the domain of \(\operatorname{colim}\) extends to the Grothendieck construction \(\int [-,\mathcal{C}]\),
which keeps track of index changes \(\mathcal{I} \to \mathcal{J}\).

## Grothendieck construction

Let \(F \colon \mathcal{D}^\mathrm{op} \to \mathrm{Cat}\) be a contravariant functor.
The **Grothendieck construction** \(\int F\) is the category where
- objects are pairs \((d,e)\) of \(d \in \mathcal{D}\) and \(e \in F(d)\),
- morphisms \((d,e) \to (d',e')\) are given by pairs \((f \colon d \to d', e \to (F f)(e'))\),
- composition is defined as \((g,\psi) \circ (f,\varphi) \coloneqq (g f, (F f)(\psi) \circ \varphi)\).

## Functoriality of colimit

Let \(\mathcal{C}\) be cocomplete.
Then
\[
  \operatorname{colim} \colon \int [-,\mathcal{C}] \to \mathcal{C}, \quad
    (\mathcal{I},F) \mapsto \operatorname{colim}(F)
\]
is a functor.
A morphism \((\mathcal{I},F) \to (\mathcal{J},G)\) consists of a functor \(H \colon \mathcal{I} \to \mathcal{J}\)
and a natural transformation \(\eta \colon F \Rightarrow [H,\mathcal{C}](G) \coloneqq G \circ H\).
[Whiskering][whiskering] the colimiting [cocone][cocone] \(\iota \colon G \Rightarrow \Delta \operatorname{colim}(G)\) with \(H\),
and precomposing with \(\eta\), we obtain the cocone
\[
    \iota H \circ \eta \colon
    F \Rightarrow G \circ H \Rightarrow (\Delta \operatorname{colim}(G)) \circ H = \Delta \operatorname{colim}(G).
\]
The universal property of \(\operatorname{colim}(F)\) implies a unique cocone morphism
\(\operatorname{colim}(H,\eta) \colon \operatorname{colim}(F) \to \operatorname{colim}(G)\).
I ran out of letters, so I will leave the preservation of composition and identity as an exercise in bookkeeping.

[cocomplete]: https://ncatlab.org/nlab/show/cocomplete+category
[cocone]: https://ncatlab.org/nlab/show/cocone
[colimit]: https://en.wikipedia.org/wiki/Limit_(category_theory)#Colimits
[functor-category]: https://en.wikipedia.org/wiki/Functor_category
[whiskering]: https://ncatlab.org/nlab/show/whiskering
