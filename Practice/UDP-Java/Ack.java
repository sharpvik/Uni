import java.net.*;


class Ack extends Message {
    Ack(InetAddress addr, int port) {
        super("ACKNOWLEDGEMENT", addr, port);
    }
}
