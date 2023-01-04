---
title: "Home"
date: 2022-12-28T19:44:42+01:00
---

I’m a student from Germany interested in different fields of mathematics and computer science. I expect to graduate from high school in the summer of 2023, after which I will pursue a degree in mathematics at the CAU in Kiel. In my free time I go bouldering, play electric guitar or develop software in C or Haskell.

This webpage is still in progress. But you can have a look at [this post][1], which I’ve basically just created to test some features and tweak the theme according to my personal preferences. My goal is to slightly adjust the colors of the [hello-friend][2] theme to match the Gruvbox Material color scheme.

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

[2]: https://github.com/panr/hugo-theme-hello-friend
[1]: /posts/just-a-test
