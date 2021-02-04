import java.io.*;
import java.net.*;
import java.util.Scanner;


/**
 * UDPSender represents connection to UDP server. There is no real 'connection'
 * going on, so Conn just stores server address and port. Conn is created for
 * its convenient send function.
 */
class UDPSender {
    private InetAddress targetAddress;
    private int targetPort;
    private DatagramSocket socket;

    /** Constructor expects target server's address and port. */
    public UDPSender(String address, int port) throws Exception {
        this.targetAddress = InetAddress.getByName(address);
        this.targetPort = port;
        this.socket = new DatagramSocket();
    }

    /** The send method only cares for the message you wish to send. */
    public String send(String message) throws Exception {
        byte[] buf = message.getBytes();
        DatagramPacket packet = new DatagramPacket(
                buf, buf.length, this.targetAddress, this.targetPort);
        this.socket.send(packet);
        return String.format("SEND DatagramPacket %s %s:%s\n",
                new String(packet.getData()),
                packet.getAddress(),
                packet.getPort());
    }
}
