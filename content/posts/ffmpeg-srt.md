---
title: "Encrypted video call with FFmpeg over SRT"
date: 2026-04-03
tags: ["ffmpeg", "linux"]
author: "Andy Sukowski-Bang"
description: "Set up an encrypted audio/video [SRT](https://en.wikipedia.org/wiki/Secure_Reliable_Transport) stream via [FFmpeg](https://ffmpeg.org/) and a [`srt-live-transmit`](https://github.com/Haivision/srt/blob/master/docs/apps/srt-live-transmit.md) relay."
---


Set up an encrypted audio/video [SRT][srt] stream via [FFmpeg][ffmpeg] and a [`srt-live-transmit`][srt-live-transmit] relay.
For a proper video call, establish connections in both directions.

## Relay

On the server, run [`srt-live-transmit`][srt-live-transmit] to relay port `5000` to `5001`.

```sh
srt-live-transmit \
  "srt://0.0.0.0:5000?mode=listener&passphrase=<pass_1>" \
  "srt://0.0.0.0:5001?mode=listener&passphrase=<pass_2>"
```

## Send

Client **A** sends microphone audio and webcam video to the server on port `5000`.

```sh
ffmpeg \
  -f alsa -i default \
  -f v4l2 -i /dev/video0 \
  -c:a aac \
  -c:v libx264 -preset fast -tune zerolatency \
  -f mpegts \
  "srt://<ip>:5000?passphrase=<pass_1>"
```

For [screen sharing][ffmpeg-desktop] on X11, replace the camera input with `x11grab`.
Add `-f alsa -i default -c:a aac` to include audio.

```sh
ffmpeg \
  -f x11grab -framerate 5 -i "$DISPLAY" \
  -c:v libx264 -preset fast -tune zerolatency \
  -x264-params repeat-headers=1 \
  -pix_fmt yuv420p \
  -crf 32 \
  -f mpegts \
  "srt://<ip>:5000?passphrase=<pass_1>"
```

## Receive

Client **B** receives the stream from `5001`.

```sh
ffplay -fflags nobuffer -flags low_delay \
  "srt://<ip>:5001?passphrase=<pass_2>"
```

[ffmpeg]: https://ffmpeg.org/
[ffmpeg-desktop]: https://trac.ffmpeg.org/wiki/Capture/Desktop
[srt]: https://en.wikipedia.org/wiki/Secure_Reliable_Transport
[srt-live-transmit]: https://github.com/Haivision/srt/blob/master/docs/apps/srt-live-transmit.md
