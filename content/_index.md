---
title: "/home/andy"
date: 2022-12-28T19:44:42+01:00
---

I’m a student from Germany interested in different fields of mathematics and
computer science. I expect to graduate from high school in the summer of 2023,
after which I will pursue a degree in mathematics at the CAU in Kiel. In my free
time I go bouldering, play electric guitar or develop software in C or Haskell.

This webpage is still in progress. But you can have a look at [this post][1],
which I’ve basically just created to test some features and tweak the theme
according to my personal preferences. And here's a function to look at.

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

## Send me some Monero

I don't know why you would, but I would definitely not complain. Here is my
[Monero (XMR) address][3]. It's the best crypto currency in existence btw, so go
ahead and use it, if you don't already.

```
83tmLoa1NWZMK15MyFT5BgGNcQRCfSBqzQekHoLxnQVEUKxcHzN44YP3jdB5ommmBmHA2yJB9zzVxiTmt3BNHB5hGh6ntmh
```

## My Linux Setup

A few years ago, I completely wiped Windows from my SSD, replacing it with
Ubuntu. Over the years, I've distrohopped to Arch, Gentoo (took way to long to
compile programs) and finally to Void Linux. I've used Gnome, KDE, qtile, i3 and
now DWM.

{{< image src="/img/linux_system.png" alt="Void Linux with DWM, LunarVim, HTOP and neofetch, Gruvbox Material theme" >}}

* **OS:** Void Linux
* **WM:** DWM from [suckless][4]
* **Terminal:** ST
* **Editor:** NeoVim

[1]: /posts/just-a-test
[2]: https://github.com/panr/hugo-theme-hello-friend
[3]: /xmr.txt
[4]: https://suckless.org
