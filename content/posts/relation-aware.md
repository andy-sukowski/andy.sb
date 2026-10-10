---
title: "Optimizing relation-aware attention"
date: 2026-10-10
tags: ["ai", "mathematics"]
author: "Andy Sukowski-Bang"
description: "We slightly modify [relation-aware attention](https://arxiv.org/abs/1803.02155) to learn edge key and edge value projection matrices. Simple rearrangements circumvent the need to compute and materialize all edge keys and edge values separately."
---

<style>
.katex-display > .katex {
  padding: 0.3em;
}
</style>

Relation-aware cross-attention extends the two input sequences (now nodes) to a directed [complete bipartite graph][bipartite].
At first, it might appear necessary to project all \(m \times n\) edge keys and edge values separately.
Two mathematically simple rearrangements avoid materializing these projections and reduce computation.

## Ordinary cross-attention

Recall [ordinary attention][attention], which trains a query matrix \(W^Q\), a key matrix \(W^K\) and a value matrix \(W^V\).
Cross-attention takes as input a primary sequence \(x_1, \dots, x_m\) and a secondary sequence \(y_1, \dots, y_n\),
which coincide in self-attention.
The query \(q_i\), key \(k_j\) and value \(v_j\) arise as matrix-vector products

\[
  q_i \coloneqq W^Q x_i, \quad
  k_j \coloneqq W^K y_j, \quad
  v_j \coloneqq W^V y_j.
\]

The compatibility of a query \(q_i\) and key \(k_j\) is their dot product,
scaled by the square root of their dimension \(d\) to [reduce variance][scaled-dot-product].

\[ c_{ij} \coloneqq \frac{q_i^\intercal k_j}{\sqrt{d}} \]

Apply [Softmax][softmax] to obtain attention scores \(\alpha_{ij} \in [0,1]\),
satisfying \(\sum_j \alpha_{ij} = 1\).

\[
\begin{aligned}
  \alpha_{ij} \coloneqq{}&
  \operatorname{softmax}_j(c_{i1}, \dots, c_{in}) \\
  ={}& \frac{\exp c_{ij}}{\sum_{k=1}^n \exp c_{ik}}
  \in [0,1].
\end{aligned}
\]

Now the \(i\)-th output embedding is the weighted sum

\[ z_i \coloneqq \sum_{j=1}^n \alpha_{ij} v_j. \]

## Relation-aware cross-attention

In (a slight generalization of) [relation-aware attention][relation-aware],
the input is a directed [complete bipartite graph][bipartite] with edges \(e_{ij}\) between nodes \(x_i\) and \(y_j\).
Additional weight matrices \(E^K\) for edge keys and \(E^V\) for edge values are trained[^learned]
and incorporated into the respective equations.

[^learned]: The [original paper][relation-aware] learns the key and value representations directly.

\[
\begin{aligned}
  c_{ij} &\coloneqq \frac{q_i^\intercal (k_j + E^K e_{ij})}{\sqrt{d}} \\
  z_i &\coloneqq \sum_{j=1}^n \alpha_{ij} (v_j + E^V e_{ij})
\end{aligned}
\]

## Optimization

At first glance, it seems necessary to compute and materialize \(m \times n\) edge keys \(E^K e_{ij}\).
But by associativity, we can instead precompute \(m\) products \(q_i^\intercal E^K\)
followed by \(m \times n\) dot products.

\[
  q_i^\intercal (E^K e_{ij})
  = (q_i^\intercal E^K) e_{ij}
\]

Similarly, we need not compute \(m \times n\) edge values \(E^V e_{ij}\).
By linearity, we can first compute \(m\) weighted sums \(\sum_j \alpha_{ij} e_{ij}\)
and thereafter multiply with \(E^V\).

\[
  \sum_{j=1}^n \alpha_{ij} (E^V e_{ij})
  = E^V \sum_{j=1}^n \alpha_{ij} e_{ij}
\]

We reduced arithmetic operations and the memory required for intermediate results.
However, we did _not_ change the asymptotic time or space complexity with respect to sequence length.
We still compute \(m \times n\) attention scores and materialize \(m \times n\) edge vectors \(e_{ij}\).

## Flax NNX Implementation

For expository purposes, this multi-head [Flax NNX][flax] implementation does not support masking.
Instead of [`nnx.LinearGeneral`][linear-general], we use [`nnx.Einsum`][einsum] to move the `heads` before the sequence dimension.

```py
import jax
import jax.numpy as jnp
from flax import nnx
from functools import partial

class RelationAwareAttention(nnx.Module):
    def __init__(
        self,
        heads: int,
        x_dim: int,
        y_dim: int,
        edge_dim: int,
        qk_dim: int,
        v_dim: int,
        out_dim: int,
        *,
        rngs: nnx.Rngs
    ):
        self.scale = jnp.sqrt(qk_dim)

        Einsum = partial(nnx.Einsum, rngs=rngs)

        # move heads dimension to later treat it as a batch dimension
        self.query = Einsum('...mx,hxd->...hmd', (heads, x_dim, qk_dim),  (heads, qk_dim))
        self.key   = Einsum('...ny,hyd->...hnd', (heads, y_dim, qk_dim))
        self.value = Einsum('...ny,hyv->...hnv', (heads, y_dim, v_dim))
        self.out   = Einsum('...hmv,hvo->...mo', (heads, v_dim, out_dim), (out_dim,))

        self.edge_key   = Einsum('...hmd,hde->...hme', (heads, qk_dim, edge_dim))
        self.edge_value = Einsum('...hme,hev->...hmv', (heads, edge_dim, v_dim))

    def __call__(
        self,
        x: jax.Array,    # (..., m, x_dim)
        y: jax.Array,    # (..., n, y_dim)
        edges: jax.Array # (..., m, n, edge_dim)
    ) -> jax.Array:
        # heads dimension right after batch dimensions
        q = self.query(x) / self.scale
        k = self.key(y)
        v = self.value(y)

        c_nodes = jnp.einsum('...md,...nd->...mn', q, k)
        c_edges = jnp.einsum('...hme,...mne->...hmn', self.edge_key(q), edges)
        c = c_nodes + c_edges

        alpha = jax.nn.softmax(c, axis=-1)

        z_nodes = jnp.einsum('...mn,...nd->...md', alpha, v)
        s = jnp.einsum('...hmn,...mne->...hme', alpha, edges)
        z_edges = self.edge_value(s)

        return self.out(z_nodes + z_edges)
```

## What about biases?

An edge key bias \(b^K\) would merely introduce a fixed summand for each \(i\) that [cancels in Softmax][softmax].
Thus we can omit the edge key bias vector \(b^K\) entirely, as is already commonly done for node keys.

\[
\begin{aligned}
  c_{ij} \coloneqq{}& \frac{q_i^\intercal (k_j + E^K e_{ij} + b^K)}{\sqrt{d}} \\
  ={}& \frac{q_i^\intercal k_j}{\sqrt{d}} + \frac{q_i^\intercal E^K}{\sqrt{d}} e_{ij} + \frac{q_i^\intercal b^K}{\sqrt{d}}
\end{aligned}
\]

An edge value bias \(b^V\) would not present a problem either, since \(\sum_j \alpha_{ij} = 1\).
But e.g. the implementation above introduces `self.out` as [motivated here][attention], which can absorb \(b^V\).

\[
\begin{aligned}
  z_i \coloneqq{}& \sum_{j=1}^n \alpha_{ij} (v_j + E^V e_{ij} + b^V) \\
  ={}& \Biggl( \sum_{j=1}^n \alpha_{ij} v_j \Biggr) + E^V \Biggl( \sum_{j=1}^n \alpha_{ij} e_{ij} \Biggr) + b^V
\end{aligned}
\]

[attention]: https://www.3blue1brown.com/lessons/attention/
[scaled-dot-product]: https://ai.stackexchange.com/a/42197
[softmax]: https://andy.sb/posts/nn-gradients/#softmax
[relation-aware]: https://arxiv.org/abs/1803.02155
[bipartite]: https://en.wikipedia.org/wiki/Complete_bipartite_graph
[flax]: https://flax.readthedocs.io/en/latest/
[linear-general]: https://flax.readthedocs.io/en/latest/api_reference/flax.nnx/nn/linear.html#flax.nnx.LinearGeneral
[einsum]: https://flax.readthedocs.io/en/latest/api_reference/flax.nnx/nn/linear.html#flax.nnx.Einsum
