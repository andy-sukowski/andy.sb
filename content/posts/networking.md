---
title: "Networking cheat sheet"
date: 2023-07-01T21:18:16+02:00
tags: ["networking", "c", "linux"]
author: "Andy Sukowski-Bang"
description: "This cheat sheet is based on Beej's Guide to Network Programming and summarizes the basics of network programming in C using `<sys/socket.h>`."
---

This cheat sheet is based on [Beej's Guide to Network Programming][1] 
and summarizes the basics of network programming in C using `<sys/socket.h>`.
I have uploaded an example [`server.c`][2] and [`client.c`][3] programm if you
want to have a look.

## Terminology

* To the kernel, all open files are referred to by **socket descriptors**, which
  are of type `int`.
* A **socket address** is the combination of an IP address and a port. An
  example of a socket address is `100.32.24.6:74`.

## Data Structures

The following structures are needed for network programming.

### `struct sockaddr` - hold socket address

This structure holds socket address information for many types of sockets.
`sa_data` contains a destination address and port number for the socket.

```c
struct sockaddr {
	unsigned short    sa_family;   /* address family, e.g. AF_INET, AF_INET6 */
	char              sa_data[14]; /* 14 bytes of protocol address           */
};
```

This is rather unwieldy since you don't want to tediously pack the address in
the `sa_data` by hand.

### `struct sockaddr_in` - parallel structure for IPv4

To deal with `struct sockaddr`, programmers created a parallel structure:
`struct sockaddr_in` (_in_ for Internet) to be used with IPv4

```c
/* IPv4 only - see `struct sockaddr_in6` for IPv6 */

struct sockaddr_in {
	short int          sin_family;  /* Address family, AF_INET      */
	unsigned short int sin_port;    /* Port number                  */
	struct in_addr     sin_addr;    /* Internet address             */
	unsigned char      sin_zero[8]; /* Same size as struct sockaddr */
};
```

This structure makes it easy to reference elements of the socket address. This
is the `sin_addr` field is a `struct in_addr`.

```c
/* internet address (a structure for historical reasons) */
struct in_addr {
	uint32_t s_addr; /* that's a 32-bit int (4 bytes) */
};
```

### `struct sockadd_in6` - parallel structure for IPv6

This is the equivalent of `struct sockaddr_in` for IPv6. The IPv6 flow
information or Scope ID fields are not that important for now.

```c
/* IPv6 only - see `struct sockaddr_in` and `struct in_addr` for IPv4) */

struct sockaddr_in6 {
	u_int16_t       sin6_family;   /* address family, AF_INET6        */
	u_int16_t       sin6_port;     /* port number, Network Byte Order */
	u_int32_t       sin6_flowinfo; /* IPv6 flow information           */
	struct in6_addr sin6_addr;     /* IPv6 address                    */
	u_int32_t       sin6_scope_id; /* Scope ID                        */
};

/* internet address (a structure for historical reasons) */
struct in6_addr {
	unsigned char   s6_addr[16]; /* IPv6 address */
};
```

### `struct sockaddr_storage` - hold IPv4 or IPv6 structure

This is another simple structure, `struct sockaddr_storage` is designed to be
large enough to hold both IPv4 and IPv6 structures. Sometimes you don't know in
advance if it's going to fill out your struct sockaddr with an IPv4 or IPv6
address. So you pass in this parallel structure, very similar to `struct
sockaddr` except larger, and then cast it to the type you need.

```c
struct sockaddr_storage {
	sa_family_t  ss_family; /* address family */

	/* all this is padding, implementation specific, ignore it */
	char      __ss_pad1[_SS_PAD1SIZE];
	int64_t   __ss_align;
	char      __ss_pad2[_SS_PAD2SIZE];
};
```

### `struct addrinfo` - prepare socket address structures

This structure is used to prepare the socket address structures for subsequent
use.

```c
struct addrinfo {
	int              ai_flags;     /* AI_PASSIVE, AI_CANONNAME, etc. */
	int              ai_family;    /* AF_INET, AF_INET6, AF_UNSPEC   */
	int              ai_socktype;  /* SOCK_STREAM, SOCK_DGRAM        */
	int              ai_protocol;  /* use 0 for "any"                */
	size_t           ai_addrlen;   /* size of ai_addr in bytes       */
	struct sockaddr *ai_addr;      /* struct sockaddr_in or _in6     */
	char            *ai_canonname; /* full canonical hostname        */

	struct addrinfo *ai_next;      /* linked list, next node         */
};
```

## Functions

This is the section where we get into the system calls (and other library
calls) that allow you to access the network functionality of a Unix box, or any
box that supports the sockets API for that matter. When you call one of these
functions, the kernel takes over and does all the work for you automagically.

### `inet_pton()` and `inet_ntop()` - manipulate IP addresses

Fortunately for you, there are a bunch of functions that allow you to
manipulate IP addresses. No need to figure them out by hand and stuff them in a
`long` with the `<<` operator.

| Function      | Description             |
| ------------- | ----------------------- |
| `inet_pton()` | presentation to network |
| `inet_ntop()` | network to presentation |

First, let’s say you have a `struct sockaddr_in ina`, and you have an IP
address that you want to store into it. The function you want to use,
`inet_pton()`, converts an IP address in numbers-and-dots notation into either
a `struct in_addr` or a `struct in6_addr` depending on whether you specify
`AF_INET` or `AF_INET6`.

```c
struct sockaddr_in sa;   /* IPv4 */
struct sockaddr_in6 sa6; /* IPv6 */

inet_pton(AF_INET, "10.12.110.57", &(sa.sin_addr));             /* IPv4 */
inet_pton(AF_INET6, "2001:db8:63b3:1::3490", &(sa6.sin6_addr)); /* IPv6 */
```

What if you have a `struct in_addr` and you want to print it in
numbers-and-dots notation? In this case, you'll want to use the function
`inet_ntop()`.

```c
/* IPv4 */

char ip4[INET_ADDRSTRLEN]; /* space to hold the IPv4 string         */
struct sockaddr_in sa;     /* pretend this is loaded with something */

inet_ntop(AF_INET, &(sa.sin_addr), ip4, INET_ADDRSTRLEN);

printf("The IPv4 address is: %s\n", ip4);


/* IPv6 */

char ip6[INET6_ADDRSTRLEN]; /* space to hold the IPv6 string         */
struct sockaddr_in6 sa6;    /* pretend this is loaded with something */

inet_ntop(AF_INET6, &(sa6.sin6_addr), ip6, INET6_ADDRSTRLEN);

printf("The address is: %s\n", ip6);
```

### `getaddrinfo()` - prepare to launch

The function will return information on a particular host name (such as its IP
address) and load up a `stuct sockaddr` for you, taking care of the gritty
details (like if it's IPv4 or IPv6).

```c
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int getaddrinfo(const char *node,     /* e.g. "www.example.com" or IP */
                const char *service,  /* e.g. "http" or port number   */
                const struct addrinfo *hints,
                struct addrinfo **res);
```

You give this function three input parameters, and it gives you a pointer to a
linked list, `res`, of results.

### `socket()` - get the file descriptor

This function gets the file desriptor for the socket. Here is the breakdown of
the `socket()` system call.

```c
#include <sys/types.h>
#include <sys/socket.h>

int socket(int domain, int type, int protocol); 
```

But what are these arguments? They allow you to say what kind of socket you
want (IPv4 or IPv6, stream or datagram, and TCP or UDP).

### `bind()` - what port am I on?

Once you have a socket, you might have to associate that socket with a port on
your local machine. The port number is used by the kernel to match an incoming
packet to a certain process’s socket descriptor. If you are only going to be
doing a `connect()`, this is probably unnecessary. Here is the synopsis for the
`bind()` system call.

```c
#include <sys/types.h>
#include <sys/socket.h>

int bind(int sockfd, struct sockaddr *my_addr, int addrlen);
```

`sockfd` is the socket file descriptor returned by `socket()`. `my_addr` is a
pointer to a `struct sockaddr` that contains information about your address,
namely, port and IP address. `addrlen` is the length in bytes of that address.

### `connect()` - hey, you!

This function connects to a remote host. Here is the synopsis for the
`connect()` function.

```c
#include <sys/types.h>
#include <sys/socket.h>

int connect(int sockfd, struct sockaddr *serv_addr, int addrlen);
```

`sockfd` is the socket file descriptor returned by the `socket()` call.
`serv_addr` is a `struct sockaddr` containing the destination port and IP
address, and `addrlen` is the length in bytes of the server address structure.

### `listen()` - will somebody please call me?

This function is used if you don't want to connect to a remote host, but want
to wait for incoming connections and handle them in some way. The process is
two steps. First you `listen()`, then you `accept()` (see below). The
`listen()` call is fairly simple.

```c
int listen(int sockfd, int backlog);
```

`sockfd` is the socket file descriptor from the `socket()` system call.
`backlog` is the number of connections allowed on the incoming queue. Incoming
connection are going to wait in this queue until you `accept()` them (see
below).

We need to call `bind()` before we call `listen()` so that the server is
running on a specific port. So if you’re going to be listening for incoming
connections, the sequence of system calls you’ll make is:

```c
getaddrinfo();
socket();
bind();
listen();
/* `accept()` goes here */
```

### `accept()` - “Thank you for calling port 3490.”

Someone far far away will try to `connect()` to your machine on a port that you
are `listen()`ing on. Their connection will be queued up waiting to be
`accept()`ed. You call `accept()` and you tell it to get the pending
connection. It’ll return to you a _brand new socket file descriptor_ to use for
this single connection! That’s right, suddenly you have _two socket file
descriptors_ for the price of one! The original one is still listening for more
new connections, and the newly created one is finally ready to `send()` and
`recv()`.

```c
#include <sys/types.h>
#include <sys/socket.h>

int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
```

`sockfd` is the `listen()`ing socket descriptor. `addr` will usually be a
pointer to a local `struct sockaddr_storage` This is where the information
about the incoming connection will go (and with it you can determine which host
is calling you from which port). `addrlen` is a local integer variable that
should be set to `sizeof(struct sockaddr_storage)` before its address is passed
to `accept()`. `accept()` will not put more than that many bytes into `addr`.
If it puts fewer in, it'll change the value of `addrlen` to reflect that.

### `send()` and `recv()` - talk to me, baby

These two functions are for communicating over stream sockets or connected
datagram sockets. If you want to use regular unconnected datagram sockets,
you’ll need to see the section on `sendto()` and `recvfrom()`, below.

The `send()` call:

```c
int send(int sockfd, const void *msg, int len, int flags);
```

`sockfd` is the socket descriptor you want to send data to (returned by
`socket()` or `accept()`). `msg` is a pointer to the data you want to send, and
`len` is the length of that data in bytes. Just set `flags` to `0`. `send()`
returns the number of bytes actually sent out - this might be less than the
number you told it to send! `-1` is returned on error.

The `recv()` call is similar in many respects:

```c
int recv(int sockfd, void *buf, int len, int flags);
```

`sockfd` is the socket descriptor to read from, `buf` is the buffer to read the
information into, `len` is the macimum length of the buffer, and `flags` can
again be set to `0`. `recv` returns the number of bytes actually read into the
buffer, or `-1` on error. The return value `0` indicates that the remote side
has closed the connection on you.

### `close()` and `shutdown()` - get outta my face

You can just use the regular Unix file descriptor `close()` function,
`socketfd` is the socket descriptor.

```c
close(sockfd);
```

The `shutdown()` function gives you a little more control over how the socket
closes. It allows you to cut off communication in a certain direction, or both
ways (just like `close()` does).

```c
int shutdown(int sockfd, int how);
```

`sockfd` is the socket file descriptor you want to shutdown, and `how` is one
of the following:

| How | Effect                                    |
| --- | ----------------------------------------- |
| `0` | further recieves are disallowed           |
| `1` | further sends are disallowed              |
| `2` | further sends and recieves are disallowed |

[1]: https://beej.us/guide/bgnet/html
[2]: server.c
[3]: client.c
