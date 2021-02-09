import java.io.*;
import java.net.*;
import java.util.Scanner;


/**
 * UDPSender represents connection to UDP server. There is no real 'connection'
 * going on, so Conn just stores server address and port. Conn is created for
 * its convenient send function.
 */
class UDPActor {
    /*
     * DatagramSocket has the timeout field which affects the behaviour of
     * UDPActor.recv.
     */
    private DatagramSocket mysocket;

    /** Use this constructor for the Sender with custom socket. */
    public UDPActor(DatagramSocket sock) { mysocket = sock; }

    /** Use this constructor for the Sender. */
    public UDPActor() throws Exception { mysocket = new DatagramSocket(); }

    /**
     * Use this constructor for the Receiver if you want it wait indefinitely
     * with no timeout.
     */
    public UDPActor(int myport) throws Exception {
        mysocket = new DatagramSocket(myport);
    }

    /** Use this constructor for a timed receiver. */
    public UDPActor(int myport, int timeout) throws Exception {
        mysocket = new DatagramSocket(myport);
        mysocket.setSoTimeout(timeout);
    }

    /**
     * The recv method blocks until the message is received or timeout reached.
     * Debug information returned as a String for convenience.
     */
    public Message recv(int capacity) throws Exception {
        byte[] buf = new byte[capacity];
        DatagramPacket packet = new DatagramPacket(buf, buf.length);
        mysocket.receive(packet);
        String msg = new String(packet.getData()).trim();
        InetAddress addr = packet.getAddress();
        int port = packet.getPort();
        return (msg.equals("/exit"))
                ? new TerminationSignal(addr, port)
                : new Message(msg, addr, port);
    }

    public Message recvWithAck(int capacity) throws Exception {
        Message msg = this.recv(capacity);
        this.send(new Ack(msg.addr, msg.port));
        return msg;
    }

    /**
     * The send method only cares for the message you wish to send.
     * Debug information returned as a String for convenience.
     */
    public void send(Message msg) throws Exception {
        byte[] buf = msg.text.getBytes();
        DatagramPacket packet = new DatagramPacket(
                buf, buf.length, msg.addr, msg.port);
        mysocket.send(packet);
    }

    /**
     * This methods sends the message and then waits for an ack/t.
     * An exception is thrown upon invalid ack/t.
     */
    public void sendAndAwaitAck(Message msg) throws Exception {
        this.send(msg);
        Message ack = this.recv(64);
        if (!ack.isAck()) throw new Exception("Invalid ack/t");
    }
}
