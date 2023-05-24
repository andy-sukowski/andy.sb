---
title: "Just a Test"
date: 2022-12-28T18:59:36+01:00
tags: ["test", "yes-it-works", "another-tag"]
author: "Andy Sukowski-Bang"
description: "I'm just testing some features and tweaking the theme according to my personal preferences. My goal is to slightly adjust the colors to match the Gruvbox Material color scheme."
math: true
---

I'm just testing some features and tweaking the theme according to my personal
preferences. My goal is to slightly adjust the colors to match the [Gruvbox
Material][2] color scheme.

## Some syntax highlighting

The following function, written in the [C programming language][1], checks for
divisibility by previously found prime numbers stored in `*primes` up to the
square root to determine whether `n` is itself a prime number.

```c 
/* check against previous primes */
bool is_prime(int n, int *primes)
{
	/* only check up to sqrt(n) */
	int max_test = (int)sqrt(n);
	for (int i = 0; primes[i] <= max_test; ++i)
		if (n % primes[i] == 0)
			return false;
	return true;
}
```

The above function can then be used to entirely fill `*primes` with `count`
prime numbers. The function `get_primes()` returns a pointer to the array.

```c
/* return list of prime numbers */
int *get_primes(int count)
{
	int *primes = malloc(count * sizeof *primes);
	primes[0] = 2;

	int i = 1, n = 3;
	while (i < count) {
		if (is_prime(n, primes)) {
			primes[i++] = n;
		}
		n = n + 1;
	}

	return primes;
}
```

## Math with KaTeX

Testing inline $a^2 + b^2 = c^2$ maths. Yeees, it finally works!

[KaTeX](katex.org) is the _fastest_ math typesetting libreary for the web. The JavaScript library renders its math synchronously and doesn't need to reflow the page.

$$ \sum_{i = 0}^n i = \frac{n (n + 1)}{2} = \frac{n^2 + n}{2} $$

## Other stuff

Hugo converts [Extended Markdown][1] to HTML so it's really easy
to create a webpage with lots of content.

### An Image

Here we have a photo of Elliot Alderson from the American
television series _Mr. Robot_ created by Sam Esmail for USA
Network:

![Elliot Alderson](/img/elliot.webp)

### Quotes
This is very nice quote by Steve Jobs:

> Taking LSD was a profound experience, one of the most
> important things in my life.

### Tables

And here we have a table containing random data.

| Syntax      | Description |
| ----------- | ----------- |
| Header      | Title       |
| Paragraph   | Text        |

Here's a simple footnote,[^1] and here's a longer one.[^bignote]

### Unordered list
* Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla
  sit amet efficitur ligula. Nunc mollis urna urna, quis
  accumsan lorem sollicitudin vitae. Donec ex massa, convallis
  non mattis in, dignissim eu augue. Proin vitae magna sit amet
  metus pretium accumsan.
  * Praesent sollicitudin at odio in lobortis. In molestie enim
    eget mauris tempus, sit amet tincidunt ex porta.
  * Sed ultricies enim maximus ex laoreet dictum.
* Nam in leo ligula. Sed a mauris a odio sagittis vestibulum sit
  amet vel orci. Donec et cursus ipsum, non gravida nisi.
  * Curabitur vitae varius dolor.
  * Donec commodo sem elit, nec pellentesque quam pulvinar ac.
  * Lorem ipsum dolor sit amet, consectetur adipiscing elit. 

[^1]: This is the first footnote.

[^bignote]: Here's one with multiple paragraphs and code.

    Indent paragraphs to include them in the footnote.

    `{ my code }`

    Add as many paragraphs as you like.

[2]: https://github.com/sainnhe/gruvbox-material
[1]: https://en.wikipedia.org/wiki/C_(programming_language)
[3]: https://www.markdownguide.org/extended-syntax/
