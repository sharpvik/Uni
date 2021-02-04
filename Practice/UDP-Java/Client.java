import java.util.*;


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
    private UDPReceiver r;
    private UDPSender s;

    private void awaitAck() {
        try {
            String _ack = this.r.recv(); // Not checking the ack for now.
            System.out.println("ACK RECEIVED\n");
        } catch (Exception _e) {
            System.out.println("ACK NOT RECEIVED: TIMEOUT\n");
        }
    }

    private String getMessage() {
        System.out.print("Enter message: ");
        return stdin.nextLine().trim();
    }

    public Client(int myport, String ackaddr, int ackport) throws Exception {
        this.stdin = new Scanner(System.in);
        this.r = new UDPReceiver(myport, 5000); // Await time limit of 5 sec.
        this.s = new UDPSender(ackaddr, ackport);
    }

    public void sendAndAwaitAck(String message) throws Exception {
        this.s.send(message);
        this.awaitAck();
    }

    /**
     * Launches Client with ack port 1234 and target port 4321. We assume that
     * both Client and Server are setup on the same machine and therefore can be
     * accessed on localhost.
     */
    public static void main(String[] args) {
        try {

            Client c = new Client(1234, "localhost", 4321);

            while (true) {
                String message = c.getMessage();
                if (message.equals(".exit")) break;
                c.sendAndAwaitAck(message);
            }

        } catch (Exception e) { e.printStackTrace(); }
    }
}
