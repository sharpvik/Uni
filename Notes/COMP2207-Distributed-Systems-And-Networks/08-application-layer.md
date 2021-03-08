# Application Layer

This is the top of the network stack. Application layer relies on the layers
below to properly function.

Some of the major application layer protocols are:

- DNS (over UDP)
- DHCP
- FTP (over TCP)
- SMTP, IMAP (email)
- HTTP (over TCP)
- QUIC (over UDP)
- RTSP/RTP (streaming)
- File server

## DHCP - How Systems Get a Dynamic IP Address

```
*-------------*                              *-------------*
| DHCP Client |                              | DHCP Server |
*-------------*                              *-------------*
       |                                            |
       |                                            |
       |                                            |
       *- ( DHCP Discover ) ----------------------> *
       |                                            |
       |                                            |
       * <------------------------- ( DHCP Offer ) -*
       |                                            |
       |                                            |
       *- ( DHCP Request ) -----------------------> *
       |                                            |
       |                                            |
       * <--------------------------- ( DHCP Ack ) -*
       |                                            |
       |                                            |
       |                                            |
      ...                                          ...
```

## Telnet

Simple unencrypted terminal emulation protocol. Practically, an unencrypted
version of SSH (nowadays, `ssh` (Secure SHell) and `scp` (Secure CoPy) replaced
Telnet).

It isn't used accross the internet now but used occasionally on local nets for
configuring old hardware.

## SMTP - Simple Mail Transfer Protocol

SMTP is a very old but _reliable_ protocol used to send email. Typically TCP
connection to a mail server, with these commands/replies:

- MAIL
- RCPT
- DATA

SMTP Authentication is an extension on the basic SMTP protocol. Original email
had none, so mail relays were used for spam.

SMTP uses TCP because we expect to be able to send a lot of data in one go, so
we want this data to travel securely.

## HTTP - Hypertext Transfer Protocol

HTTP is a text-based protocol. It uses TCP to connect to server IP on port `80`
(normally). Requests are literal strings of text like this:

|    `GET`     | `/api/user/123` |    `HTTP/1.1`    |
| :----------: | :-------------: | :--------------: |
| Request type |   Request URI   | Protocol version |

We expect the response to be something like this:

|    `HTTP/1.1`    |        `200`         |          `OK`           |
| :--------------: | :------------------: | :---------------------: |
| Protocol version | Response Status Code | Response Status Message |

Followed by headers and some data (like JSON or HTML).

The `200 OK` response is the default if everything went well. There are also
infamous `403 Forbidden`, `404 Not Found` and `500 Internal Server Error`.

Typicall headers are

- Content-Type
- Date
- Content-Encoding
- Content-Length
- Last-Modified
- Server

### The Main HTTP Requests

- GET - request some data
- HEAD - like GET but no data sent back (check status and changes)
- POST - send new data to the server
- PUT - meant to change the resource on the server
- DELETE - removes a resource

### HTTP Status Codes

`1XX` - Info
`2XX` - Success
`3XX` - Redirect
`4XX` - Client Error (e.g. `403 Forbidden`, `404 Not Found`)
`5XX` - Server Error (e.g. `500 Internal Server Error`)

### PUT vs POST

`PUT /phones/kirk` could enter the number or change it.

`POST /questions` could add the sent data to the questions resource. If you do
it more than once, you'll add more questions.

### HTTP/2

First major revision to HTTP, published in 2015. It allows

- Header compression -- now binary using HPACK
- Server push (sending data _to us_)
- Multiplexing multiple requests over a single TCP connection (like if you have
  a page that has multiple images, you might as well just request them all
  together, instead of one by one)

## QUIC - Quick UDP Internet Connections

Uses the UDP protocol to make web/etc faster than TCP.

QUIC is _really a transport protocol_ -- but it is in user-space so it can be
easily upgraded. Google pushed it forward so lots of traffic is now QUIC.

> QUIC is currently _experimental_ but will soon get out of that stage.

- Visit [this page in Chrome](chrome://flags/#enable-quic) to enable QUIC
- And [this page](chrome://net-internals/#quic) lets you see your stats
- The [net-internals page](chrome://net-internals) is interesting anyways

> Use wireshark to see QUIC protocol if you visit google search.

## Constrained Application Protocol

CoAP provides HTTP-like protocol for simpler devices.

- Minimal overhead. Good for IoT.
- Binary GET/PUT/etc
- Simple discovery mechanism
- Simple Subscribe method

## Real Time Audio/Video Streaming

Video frames can be "packetized" so that losses do not corrupt the whole stream.
TCP or UDP can be used for _transport_ (where UDP is clearly a better choice).
HTTP can be used to stream media files too...

There is also the Real Time Streaming Protocol (RTSP). It has a URL scheme with
arguments:

```
rtsp://catvideos.org/media#t=10
```

Used by YouTube to Flash players. Real-time transport protocol (RTP) delivers
the media (typically over UDP).

## File Sharing

### SMB

Server Message Block (SMB) made my Microsoft. TCP port `445` or UDP ports. SMB
provides authentication, file locking, etc. Samba in Linux provides it.

### NFS

NFS is used mainly in Unix/Linux. It is common protocol for between-servers
communication and so it is capable of coping with very large files and file
numbers.

### P2P

Peer-to-peer can distribute content efficiently. BitTorrent (2001) does this for
files, transfering them in chunks of 86 to 512 kB. There is usually a tracker
(server) that maintains a list of content. Clients get and send blocks of data
within the network (decentralised data sharing).
