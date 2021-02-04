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
