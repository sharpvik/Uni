import java.net.*;


/**
 * Message contains information about the port and network address alongside
 * with the message text sent or received.
 */
class Message {
    public String text;
    public InetAddress addr;
    public int port;

    public Message(String text, InetAddress addr, int port) {
        this.text = text;
        this.addr = addr;
        this.port = port;
    }

    public String info() {
        return String.format("[%s:%d] %s", addr, port, text);
    }

    public boolean isAck() {
        return text.equals("ACKNOWLEDGEMENT");
    }
}
