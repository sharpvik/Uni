import java.util.*;


/**
 * Server represents the server-side UDP listener. It indefinitely listens
 * to incoming messages and only shuts down on the /exit command from the
 * client. Debug information is printed to stdout for convenience.
 *
 * This version of Server supports message acknowledgement feature. As you can
 * see, we had to implement it ourselves since UDP does not support that feature
 * out of the box unlike TCP.
 *
 * Run Server with the following command:
 *
 * ~$ java Server
 *
 */
class Server {
    private UDPReceiver r;
    private UDPSender s;

    private void ack() throws Exception {
        this.s.send("ack");
    }

    public Server(int myport, String ackaddr, int ackport) throws Exception {
        this.r = new UDPReceiver(myport);
        this.s = new UDPSender(ackaddr, ackport);
    }

    public String recvWithAck() throws Exception {
        String debugInfo = this.r.recv();
        this.ack();
        return debugInfo;
    }

    /**
     * Launches Server on port 4321 with ack port 1234. We assume that both
     * Client and Server are setup on the same machine and therefore can be
     * accessed on localhost.
     */
    public static void main(String[] args) {
        System.out.println("Serving at localhost:1234 ...\n");
        try {

            Server s = new Server(4321, "localhost", 1234);

            while (true) {
                String debugInfo = s.recvWithAck();
                if (debugInfo.equals("/exit")) break;
                System.out.println(debugInfo);
            }

        } catch (Exception e) { e.printStackTrace(); }

        System.out.println("SIGINT: EXIT");
    }
}
