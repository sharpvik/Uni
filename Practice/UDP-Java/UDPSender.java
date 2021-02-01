import java.io.*;
import java.net.*;
import java.util.Scanner;


/**
 * UDPSender contains the main method that launches the little REPL-ish
 * interface that asks for message input and dumps it over the UDP Conn.
 *
 * Make sure to pass server address as a command-line argument like so:
 *
 * ~$  java UDPSender myserver.co.uk
 *
 */
class UDPSender {
    private static String getMessage(Scanner stdin) {
        System.out.print("Enter message: ");
        return stdin.nextLine().trim();
    }

    public static void main(String[] args) {
        Scanner stdin = new Scanner(System.in);

        try {

            Conn conn = new Conn(args[0], 4321);

            while (true) {
                String message = UDPSender.getMessage(stdin);
                if (message.equals(".exit")) break;
                String debugInfo = conn.send(message);
                System.out.println(debugInfo);
            }

        } catch (Exception e) { e.printStackTrace(); }
    }
}


/**
 * Conn represents connection to UDP server. There is no real 'connection'
 * going on, so Conn just stores server address and port. Conn is created for
 * its convenient send function.
 */
class Conn {
    private InetAddress targetAddress;
    private int targetPort;
    private DatagramSocket socket;

    /** Constructor expects target server's address and port. */
    public Conn(String address, int port) throws Exception {
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
