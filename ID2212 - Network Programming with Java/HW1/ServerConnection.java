
package se.kth.id2212.hw1;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * Handles all communication with the hangman server.
 * 
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 * 
 */
public class ServerConnection implements Runnable
{
    private final String host;
    private final int port;
    private final HangmanClient gui;
    private final LinkedBlockingQueue<String> strings =
            new LinkedBlockingQueue<>();
    private BufferedInputStream in;
    private BufferedOutputStream out;
    private Socket clientSocket;
    
    
    /**
     * Creates a new instance. Does not connect to the server.
     *
     * @param gui  The client gui object.
     * @param host The reverse server host name.
     * @param port The reverse server port number.
     */
    public ServerConnection(HangmanClient gui, String host, int port)
    {
        this.host = host;
        this.port = port;
        this.gui = gui;
    }

    /**
     * The run method of the communication thread. First connects to
     * the server using the host name and port number specified in the
     * constructor. Second waits for some input from the user and sends it 
     * to the server and does it forever.
     */
    @Override
    public void run()
    {
        connect();
        callServer();
    }

    /**
     * Connects to the server using the host name and port number
     * specified in the constructor.
     */
	protected void connect() {
		try {
			clientSocket = new Socket(host, port);
			in = new BufferedInputStream(clientSocket.getInputStream());
			out = new BufferedOutputStream(clientSocket.getOutputStream());
			gui.connected();
		} catch (UnknownHostException e) {
			System.err.println("Don't know about host: " + host + ".");
			System.exit(1);
		} catch (IOException e) {
			System.err.println("Couldn't get I/O for the connection to: "
					+ host + ".");
			System.exit(1);
		}
	}

    /**
     * Used to submit a string for guessing.
     *
     * @param text - guess.
     */
    void sendMessage(String text)
    {
        strings.add(text);
    }

    /**
     * Waits to receive a string from the gui and sends that to the
     * hangman server.
     */
    void callServer() {
        try {
        	byte[] toServer = strings.take().getBytes();
            out.write(toServer, 0, toServer.length);
            out.flush();
            
            byte[] fromServer = new byte[1024];
			int bytesRead = 0;
			int n;
			while ((n = in.read(fromServer, bytesRead, 256)) != -1) {
				bytesRead += n;
				if (bytesRead == 4096) {
					break;
				}
				if (in.available() == 0) {
					break;
				}
			}

			
			// Debug
			String response = new String(fromServer, 0, bytesRead);
			System.out.println("Received from server: " + response);
			
			String[] messages = response.split("\\|");
			switch (messages[0]) {
			case "NEWGAME":
				gui.gameStarted();
				break;
			case "FAIL":
				gui.changeImage(messages[3]);
				gui.appendGuess(new String(toServer));
				break;
			case "WIN": 
				gui.showCongratulation();
				gui.gameEnded();
				break;
			case "LOSS":
				gui.changeImage(messages[3]);
				gui.showLoss();
				gui.gameEnded();
				break;
			}
			gui.updateGame(messages[1], messages[2], messages[3]);
	        if (clientSocket.isConnected())
	        	callServer();
        } catch (InterruptedException | IOException e) {
        	e.printStackTrace();
        } 
    }
}
