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
    private UDPActor udp;

    public Server(int myport) throws Exception {
        udp = new UDPActor(myport);
    }

    public Message recvWithAck() throws Exception {
        return udp.recvWithAck(1024);
    }

    /**
     * Launches Server on port 4321.
     * We assume that both Client and Server are setup on the same machine and
     * therefore can beaccessed on localhost.
     */
    public static void main(String[] args) {
        int port = 4321;
        System.out.printf("Serving at localhost:%d ...\n", port);

        try {

            Server s = new Server(port);

            while (true) {
                Message msg = s.recvWithAck();
                System.out.println(msg.info());
                if (msg instanceof TerminationSignal) break;
            }

        } catch (Exception e) { e.printStackTrace(); }
    }
}
