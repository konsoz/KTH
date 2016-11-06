package se.kth.id2212.hw1;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.net.Socket;

/**
 * A connection handler for the Hangman server. Each connection handles one
 * client and therefore one game.
 * 
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 * 
 */
public class ConnectionHandler implements Runnable {
    private static final int DELAY_SECS = 1;
    private static final int SECS_TO_MILLIS = 1000;
    private Socket clientSocket;
    private GameHandler gameHandler;

	/**
	 * Creates a new instance and initializes a new game.
	 *
	 * @param clientSocket
	 *            The socket that is associated with this connections client.
	 */
    ConnectionHandler(Socket clientSocket) {
        this.clientSocket = clientSocket;
        this.gameHandler = new GameHandler();
    }

	/**
	 * Reads a string from the client, processes it and returns the response to
	 * the client.
	 */
	@Override
	public void run() {
        try (BufferedInputStream in = new BufferedInputStream(clientSocket.getInputStream());
             BufferedOutputStream out = new BufferedOutputStream(clientSocket.getOutputStream())) {
            byte[] msg = new byte[4096];
			int bytesRead = 0;
			int n;
			while ((n = in.read(msg, bytesRead, 256)) != -1) {
				bytesRead += n;
				if (bytesRead == 4096) {
					break;
				}
				if (in.available() == 0) {
					break;
				}
			}

			// Debug code
			String fromClient = new String(msg, 0, bytesRead);
            System.out.println("Received: " + fromClient);
            
            // Process the input
            String output = gameHandler.processInput(fromClient);

            Thread.sleep(DELAY_SECS * SECS_TO_MILLIS);

			byte[] toClient = output.getBytes();
			out.write(toClient, 0, toClient.length);
			out.flush();
			run();
		} catch (Exception e) {
			System.out.println(e.getMessage() + " on port: " + clientSocket.getPort());
		}
	}
}
