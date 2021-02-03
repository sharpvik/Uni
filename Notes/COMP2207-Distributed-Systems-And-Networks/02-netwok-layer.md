# Network Layer

The **Network Layer** is responsible for IP protocols and all things related to
communication over the internet. It provides services to the Transport Layer.

Put simply, it does _internetworking_ by hiding routing from the Transport
Layer.

- Packetising (adding the IP header to the Transport data)
- Processing and Routing IP diagrams
- Fragmenting (where IP diagram size > link layer MTU)

**MTU** stands for Max Transmission Unit.

It is also responsible for receiving, including error checking and reassembly
of fragmented packets.

## IP Properties

This packet-switched environment is **connectionless**. Meaning, it's not too
reliable, and somethimes packets are lost. Routing is done based only on the
destination IP address where routers maintain routing tables to make routing
decisions.

The IP addresses are unique addresses of computers on the network. Your device
must be globalyl addressible to receive traffic.

In this network, we deal with IPv4 and IPv6, ICMP and ICMPv6 (responsible for
diagnostics and control like ping), IPSEC (security), IGMP.

## Store-and-Forward Packet Switching

Each node on the network takes a package, saves it, and forwards it to the
destination. This architecture means that our machines can be network-agnostic.
You don't have to know anything about the middleman computers that deliver your
data to the final destination.

## Unreliability

Routers forward packets on 'best effort' basis. The whole thing is really
**unreliable** IP packets may be dropped, usually due to congestion, but you
can attempt to prioritise traffic.

### TCP vs UDP

TCP is connection-based so retransmissions are handled by the protocol itself.
UDP is connectionless so retransmission must be handled by the Application
Layer.

## The IPv4 Packet Header

Total length: 32 bits.

- Version (4/6)
- Length of header
- Type of service (priority, forwarding, classification)
- Total length of the packet + header
- Identification of fragments (number order)
- Fragment offset
- Time to live (hop count until packet death)
- Protocol field (UDP/TCP/ICMP or something else)
- Header checksum for error detection
- Source IP
- Destination IP
- Optional (MTU probe, time stamp, traceroute)

## Fragmentation

This is something you want to avoid for efficiency reasons of fragmenting and
defragmenting packets + some security reasons.

## IPv4 Problems and Solutions

We've ran out of IPv4 addresses in 2011 and we're now desperate to solve this
problem. For example:

**Dinamic Host Configuration Protocol (DHCP)** allows IP addresses to be
_reused_.

**Network Address Translation (NAT)** where multiple private network addresses
are seen from the outside as the same public address. This means that your
private IP address is not publically visible. This is very popular. You
definitely got NAT in your house.

## Subnets

It is sometimes useful to split all IP addresses based on departments or
divisions and let a local router deal with them in a clever way.

## IPv4 Allocations

There are different allocations based on how many free addresses a company or
some other entity has available to them. These are written like `/8`, `/16`, and
`/24` to specify how many prefix bits are _fixed_.

So if we have `152.48.0.0/16`, we know that the last two bytes are for us to
play with, which gives us 255<sup>2</sup> addresses to play with.

This, however is a very _generous_ way of spreading the IP love around. If you
need 500 addresses, you will not be able to get anything close to that and you
will need to have the `/16` allocation with 65K addresses. This is a great way
to run out of available addresses!

## Classless Inter Domain Routing (CIDR)

**Classless Inter Domain Routing (CIDR)** allows very specific address
allocations and routing based on those allocations.

A site or ISP requesting address space gets almost exactly what it needs. You
can now have `/23` allocation with 512 addresses available instead of wasting
65K of addresses for someone who needs 1K!

Many sites still have their original generous allocations. For example, MIT
still has `18.0.0.0/8` with 16M addresses available to them. They'll never have
enough computers to fill this address space.

Today, new applicants in the European (RIPE) region get at most a `/22` address
block (1024 addresses).

## CIDR Classes

There are site classes labelled A-E based on the allocation they have.

## Address Resolution Protocol (ARP)

It uses subnet masks to quickly resolve a MAC address of the target machine on
the local network. It works kind of like this:

> -- Who has this IP here? *specifies the IP it's looking for*
>
> -- I do! Here's my MAC address! *gives him the MAC*

But if the destination IP is not in the same subnet, the sender should ARP for the Ethernet address of its default router. Then, the router will deal with
everything else.

This is why netmasks are very important. The heuristic is: "Two machines with
the same netmasks **must be on the same network**!"

## Internet Control Message Protocol (ICMP)

If you `ping` a host, you send it an ICMP echo request packet.
