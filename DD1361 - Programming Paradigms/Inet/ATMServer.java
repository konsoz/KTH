import java.net.*;
import java.util.ArrayList;
import java.util.List;
import java.io.*;

/**
   @author Viebrapadata
*/
public class ATMServer {

    private static int connectionPort = 8989;
    
    public static void main(String[] args) throws IOException {
        
        ServerSocket serverSocket = null;
        Socket conn = null;
        
        // Some test clients
        
        List <Account> clients = new ArrayList <Account>();
        
        List <Integer> client1Codes = new ArrayList <Integer>();
        client1Codes.add(1);
        client1Codes.add(3);
        client1Codes.add(5);
        List <Integer> client2Codes = new ArrayList <Integer>();
        client2Codes.add(7);
        client2Codes.add(9);
        client2Codes.add(11);
        List <Integer> client3Codes = new ArrayList <Integer>();
        client3Codes.add(13);
        client3Codes.add(15);
        client3Codes.add(17);
        
        clients.add(new Account(1234, 11, 1000, client1Codes,"Daniel"));
        clients.add(new Account(5678, 22, 800, client2Codes,"Konstantin"));
        clients.add(new Account(9012, 22, 500, client3Codes,"Tim"));
        
        boolean listening = true;
        
        // Initialize server socket
        
        try {
            serverSocket = new ServerSocket(connectionPort); 
        } catch (IOException e) {
            System.err.println("Could not listen on port: " + connectionPort);
            System.exit(1);
        }
        
        // Initialize one socket for every user
        
        System.out.println("Bank started listening on port: " + connectionPort);
        while (listening) {
        	conn = serverSocket.accept();
        	System.out.println("Connection received from " + conn.getInetAddress().getHostName() + " : " + conn.getPort());
        	new ATMServerThread(conn, clients).start();
        	
        }
            

        serverSocket.close();
    }
}
