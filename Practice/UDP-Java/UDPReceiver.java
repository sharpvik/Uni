import java.io.*;
import java.net.*;


class UDPReceiver {
    private DatagramSocket socket;

    /** This is a simple constructor with indefinite timeout on the wait. */
    public UDPReceiver(int myport) throws Exception {
        this.socket = new DatagramSocket(myport);
    }

    /** This constructor includes timeout for the blocking socket wait. */
    public UDPReceiver(int myport, int timeout) throws Exception {
        this.socket = new DatagramSocket(myport);
        this.socket.setSoTimeout(timeout);
    }

    /**
     * The recv method blocks until the message is received or timeout reached.
     */
    public String recv() throws Exception {
        byte[] buf = new byte[1000];
        DatagramPacket packet = new DatagramPacket(buf, buf.length);
        this.socket.receive(packet);
        String message = new String(packet.getData()).trim();
        return (message.equals("/exit"))
                ? "/exit"
                : String.format("RECV DatagramPacket %s %s:%s\n",
                  message, packet.getAddress(), packet.getPort());
    }
}
