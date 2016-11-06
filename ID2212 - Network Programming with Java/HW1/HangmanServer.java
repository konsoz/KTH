package se.kth.id2212.hw1;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * The main class of the Hangman server. Opens a listening socket and waits for
 * client connections. Each connection is managed in a separate thread.
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 * 
 */
public class HangmanServer {
	private static final int PORT = 4444;

	/**
	 * Starts the hangman server.
	 *
	 * @param args
	 *            Not used.
	 */
	public static void main(String[] args) {
		boolean listening = true;
		ServerSocket serverSocket;

		try {
			serverSocket = new ServerSocket(PORT);
			System.out.println("Server is listening on port: " + PORT);
			while (listening) {
				Socket clientSocket = serverSocket.accept();
				new Thread(new ConnectionHandler(clientSocket)).start();
			}
			serverSocket.close();
		} catch (IOException e) {
			System.err.println("Could not listen on port: " + PORT);
			System.exit(1);
		}
	}
}
