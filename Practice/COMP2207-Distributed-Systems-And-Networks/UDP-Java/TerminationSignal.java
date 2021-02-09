import java.net.*;


/**
 * TerminationSignal is a special Message sent over the network to kill a
 * process. It's just a convenience.
 */
class TerminationSignal extends Message {
    public TerminationSignal(InetAddress addr, int port) {
        super("SIGTERM", addr, port);
    }
}
