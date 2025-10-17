---
title: "Neural Network Gradients"
date: 2025-10-17
tags: ["ai", "math"]
author: "Andy Sukowski-Bang"
description: "This is a cheat sheet of forward passes and gradients for common neural network layer types: dense layer, convolutional layer, LSTM, Softmax. Everything is written in index form rather than vectorized notation to keep the barrier to entry low."
---

This is a cheat sheet of forward passes and gradients for common neural network layer types,
essentially derived using the [chain rule][chain-rule].
If you find any mistakes, please contact me at [me@andy.sb](mailto:me@andy.sb).
You can check out [my own implementations][nn-lib], written in [Julia][julia].

* [Dense Layer](#dense-layer)
* [Convolutional Layer](#convolutional-layer)
* [LSTM](#lstm)
* [Softmax](#softmax)

I've written everything in index form rather than vectorized notation to keep the barrier to entry low.
Throughout the article,
let \(\odot\) denote the [Hadamard product][hadamard],
and \(\mathcal{L}\) the network loss.

## Dense Layer

**Recommendation:**
the first 4 lessons of
["Neural Networks"](https://www.3blue1brown.com/topics/neural-networks)
by 3Blue1Brown.

Let \(x \in \mathbb{R}^n\) be the input vector,
\(y \in \mathbb{R}^m\) the output vector,
\(W \in \mathbb{R}^{m \times n}\) the weight matrix,
\(b \in \mathbb{R}^m\) the bias vector,
and \(\sigma\) an element-wise activation function.
Define the pre-activation values as \(z \coloneqq b + Wx\), i. e.
\[
    z_k \coloneqq
    b_k + \sum_{j=1}^n w_{kj} \cdot x_j
\]
for the forward pass \(y_k \coloneqq \sigma(z_k)\).
We can now calculate the gradient:
\[
    \begin{aligned}
        \frac{\partial \mathcal{L}}{\partial z_k}
        &= \frac{\partial \mathcal{L}}{\partial y_k} \cdot \sigma'(z_k) \\
        \frac{\partial \mathcal{L}}{\partial b_k}
        &= \frac{\partial \mathcal{L}}{\partial z_k} \\
        \frac{\partial \mathcal{L}}{\partial w_{kj}}
        &= \frac{\partial \mathcal{L}}{\partial z_k} \cdot x_j \\
        \frac{\partial \mathcal{L}}{\partial x_j}
        &= \sum_{k=1}^m \frac{\partial \mathcal{L}}{\partial z_k} \cdot w_{kj}.
    \end{aligned}
\]

## Convolutional Layer

**Recommendation:**
["Convolutional Neural Network from Scratch"](https://youtu.be/Lakz2MoHy6o)
by The Independent Code.

Let \(X_j\) for \(j \le n\) be the input feature maps,
\(Y_k\) for \(k \le m\) the output feature maps,
\(K_{kj}\) the corresponding kernels,
\(B_k\) the full bias maps,
and \(\sigma\) an element-wise activation function.
Let \(\star\) denote the [cross-correlation][cross-correlation] operator
and \(*\) the [convolution][convolution] operator.
Define the pre-activation map
\[
    Z_k \coloneqq
    B_k + \sum_{j=1}^n X_j \star K_{kj}
\]
for the forward pass \(Y_k \coloneqq \sigma(Z_k)\).
The gradient is
\[
    \begin{aligned}
    	\frac{\partial \mathcal{L}}{\partial Z_k}
    	&= \frac{\partial \mathcal{L}}{\partial Y_k} \odot \sigma'(Z_k) \\
    	\frac{\partial \mathcal{L}}{\partial B_k}
    	&= \frac{\partial \mathcal{L}}{\partial Z_k} \\
    	\frac{\partial \mathcal{L}}{\partial K_{kj}}
    	&= X_j \star \frac{\partial \mathcal{L}}{\partial Z_k} \\
    	\frac{\partial \mathcal{L}}{\partial X_j}
    	&= \sum_{k=1}^m \frac{\partial \mathcal{L}}{\partial Z_k} * K_{kj}.
    \end{aligned}
\]

## LSTM

**Recommendation:**
["Understanding LSTM Networks"](https://colah.github.io/posts/2015-08-Understanding-LSTMs/)
by Christopher Olah.

For time \(t\),
let \(x_t\) denote the input vector,
\(f_t\) the forget gate's activation,
\(i_t\) the input gate's activation,
\(o_t\) the output gate's activation,
\(n_t\) the new cell candidate,
\(c_t\) the cell state,
and \(h_t\) the hidden state.
Let \(\sigma_f\), \(\sigma_i\), \(\sigma_o\), \(\sigma_n\), \(\sigma_h\) be the corresponding element-wise activation functions.
\([h_{t-1},x_t]\) denotes the concatenation of \(h_{t-1}\) and \(x_t\).

\[
    \begin{aligned}
        f_t &\coloneqq \sigma_f(W_f \cdot [h_{t-1}, x_t] + b_f) \\
        i_t &\coloneqq \sigma_i(W_i \cdot [h_{t-1}, x_t] + b_i) \\
        o_t &\coloneqq \sigma_o(W_o \cdot [h_{t-1}, x_t] + b_o) \\
        n_t &\coloneqq \sigma_n(W_c \cdot [h_{t-1}, x_t] + b_c) \\
        c_t &\coloneqq f_t \odot c_{t-1} + i_t \odot n_t \\
        h_t &\coloneqq o_t \odot \sigma_h(c_t)
    \end{aligned}
\]
The gradient with respect to the cell state is given by
\[
    \begin{aligned}
        \frac{\partial \mathcal{L}}{\partial c_t}
        &= \frac{\partial \mathcal{L}}{\partial h_t}
        \odot \frac{\partial h_t}{\partial c_t}
        + \frac{\partial \mathcal{L}}{\partial c_{t+1}}
        \odot \frac{\partial c_{t+1}}{\partial c_t} \\
        &= \frac{\partial \mathcal{L}}{\partial h_t}
        \odot \sigma_h'(c_t) \odot o_t
        + \frac{\partial \mathcal{L}}{\partial c_{t+1}}
        \odot f_{t+1},
    \end{aligned}
\]
and the gradient with respect to the gates and the new cell candidate is
\[
    \begin{aligned}
        \frac{\partial \mathcal{L}}{\partial f_t}
        &= \frac{\partial \mathcal{L}}{\partial c_t} \odot c_{t - 1} \\
        \frac{\partial \mathcal{L}}{\partial i_t}
        &= \frac{\partial \mathcal{L}}{\partial c_t} \odot n_t \\
        \frac{\partial \mathcal{L}}{\partial o_t}
        &= \frac{\partial \mathcal{L}}{\partial h_t} \odot \sigma_h(c_t) \\
        \frac{\partial \mathcal{L}}{\partial n_t}
        &= \frac{\partial \mathcal{L}}{\partial c_t} \odot i_t.
    \end{aligned}
\]

## Softmax

**Recommendation:**
["The Softmax function and its derivative"](https://eli.thegreenplace.net/2016/the-softmax-function-and-its-derivative/)
by Eli Bendersky.

Let \(x \in \mathbb{R}^n\) denote the input vector,
\(y \in \mathbb{R}^n\) the output vector,
and \(c \in \mathbb{R}\) a constant for numerical stability.
Then
\[
    \begin{aligned}
    	y_k &\coloneqq \frac{e^{x_k}}{\sum_{j=1}^{n} e^{x_j}} \\
    	    &= \frac{e^{x_k + c}}{\sum_{j=1}^{n} e^{x_j + c}}.
    \end{aligned}
\]
Let \(\delta_{kj}\) denote the [Kronecker delta][kronecker].
The Softmax derivative ([Jacobian matrix][jacobian]) is
\[
	\frac{\partial y_k}{\partial x_j}
	= y_k \cdot (\delta_{kj} - y_j),
\]
so the gradient is given by
\[
	\frac{\partial \mathcal{L}}{\partial x_j}
	= \sum_{k=1}^n \frac{\partial \mathcal{L}}{\partial y_k} \cdot y_k \cdot (\delta_{kj} - y_j).
\]

[chain-rule]: https://en.wikipedia.org/wiki/Chain_rule
[convolution]: https://en.wikipedia.org/wiki/Convolution
[cross-correlation]: https://en.wikipedia.org/wiki/Cross-correlation
[hadamard]: https://en.wikipedia.org/wiki/Hadamard_product_(matrices)
[jacobian]: https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant
[julia]: https://julialang.org/
[kronecker]: https://en.wikipedia.org/wiki/Kronecker_delta
[nn-lib]: https://git.andy.sb/nn
