# UDP Service Diagram

## Overview

```
  SENDER:1234                               RECEIVER:4321
       |                                          |
       |                                          |
       |                                          |
       |- (message) ----------------------------> *
       |                                          |
       |                                          |
       * <-------------------- (acknowledgement) -|
       |                                          |
       |                                          |
      ...                                        ...
```

Both `SENDER` and `RECEIVER` communicate via the UDP protocol. `RECEIVER` sends
acknowledgement of every message received from `SENDER`. The `SENDER` may not
proceed to send the next message without receiving the ack/t for the previous
one.

## Outcomes

The acknowledgement constraint means that if you send a message before starting
the `RECEIVER`, the `SENDER` will forever loop, awaiting the ack/t unless we
introduce some concept of await time limit.

To introduce time limit, I've added a second constructor to the `UDPReceiver`
class -- it accepts an `int timeout` as a second argument. This constructor is
used by the `Client`. In the `Client.awaitAck` method, we take advantage of that
timeout to communicate whether ack/t is received or not.

### Multiple Clients

Current architecture doesn't support multiple clients since they will collide
on the port, but if they attempt to use different ports, then ack/t will only be
sent to the one that uses port 1234. This is said, but we may be able to fix
that in the next revision.

According to [this doc ref][1], it is possible to get the location of a
`DatagramSocket` created with the empty constructor that binds to any available
port. This can be used to remove the necessity of creating an extra receiver
within the `Client`. The `Server` can respond to the socket from which it
received a message (I hope).

[1]: https://docs.oracle.com/javase/7/docs/api/java/net/DatagramSocket.html#setSoTimeout(int)
