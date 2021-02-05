import java.util.*;
import java.net.*;


/**
 * Client contains the main method that launches the little REPL-ish interface
 * that asks for message input and dumps it over the UDP Conn.
 *
 * Run Client with the following command:
 *
 * ~$ java Client
 *
 */
class Client {
    private Scanner stdin;
    private InetAddress addr;
    private int port;
    private UDPActor udp;

    private String getMessage() {
        System.out.print("Enter message: ");
        return stdin.nextLine().trim();
    }

    /** Client sends messages to Server located at addr:port */
    public Client(String address, int port, int timeout) throws Exception {
        stdin = new Scanner(System.in);
        addr = InetAddress.getByName(address);
        this.port = port;
        DatagramSocket sock = new DatagramSocket();
        sock.setSoTimeout(timeout);
        udp = new UDPActor(sock);
    }

    /** Returns boolean flag to signify ack/t success/failure where
     * true = success, false = failure.
     */
    public boolean sendAndAwaitAck(String msg) {
        try {
            udp.sendAndAwaitAck(new Message(msg, addr, port));
            return true;
        } catch (Exception e) {
            System.out.println(e);
            return false;
        }
    }

    /**
     * Launches Client with target port 4321 and timeout of 2 seconds.
     * We assume that both Client and Server are setup on the same machine and
     * therefore can be accessed on localhost.
     */
    public static void main(String[] args) {
        try {

            Client c = new Client("localhost", 4321, 2000);

            while (true) {
                String msg = c.getMessage();
                if (msg.equals(".exit")) break;
                boolean ack = c.sendAndAwaitAck(msg);
                System.out.println(
                        (ack)
                        ? "ACKNOWLEDGEMENT RECEIVED\n"
                        : "ACKNOWLEDGEMENT NOT RECEIVED\n");
            }

        } catch (Exception e) { e.printStackTrace(); }
    }
}
