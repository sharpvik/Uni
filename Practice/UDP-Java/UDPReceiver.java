import java.io.*;
import java.net.*;


/**
 * UDPReceiver represents the server-side UDP receiver. It indefinitely listens
 * to incoming messages and only shuts down on the /exit command from the
 * client. Debug information is printed on the screen for convenience.
 *
 * UDPReceiver listens on port 4321.
 */
class UDPReceiver {
    private static String recv(DatagramSocket socket) throws Exception {
        byte[] buf = new byte[1000];
        DatagramPacket packet = new DatagramPacket(buf, buf.length);
        socket.receive(packet);
        String message = new String(packet.getData()).trim();
        return (message.equals("/exit"))
                ? "/exit"
                : String.format("RECV DatagramPacket %s %s:%s\n",
                  message, packet.getAddress(), packet.getPort());
    }

    public static void main(String[] args) {
        try {

            DatagramSocket socket = new DatagramSocket(4321);

            while (true) {
                String debugInfo = UDPReceiver.recv(socket);
                if (debugInfo.equals("/exit")) break;
                System.out.println(debugInfo);
            }

            System.out.println("SIGINT: EXIT");

        } catch (Exception e) { e.printStackTrace(); }
    }
}
