import java.io.*;
import java.net.*;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * 
 * This thread handling every user connection.
 * 
   @author konstantin
*/
public class ATMServerThread extends Thread {
    private Socket socket = null;
    private BufferedReader in;
    PrintWriter out;
    private List <Account> clients;
    
    public ATMServerThread(Socket socket, List <Account> clients) {
        super("ATMServerThread");
        this.socket = socket;
        this.clients = clients;
    }

  
    /**
     * This method splits string into 10 bytes chunks and writes it to the socket
     * 
     * @param toSend - string to send over socket
     * @param out - output writer
     */
    private void write10Bytes(String toSend, PrintWriter out) {
    	StringBuilder str = new StringBuilder(toSend);
    	while(str.length() > 5) {
    		String send = str.substring(0, 5);
    		out.print(send);
    		str.delete(0, 5);
    	}  		
    	out.println(str.toString());
    }

   

    public void run(){
         
        try {
            out = new PrintWriter(socket.getOutputStream(), true);
            in = new BufferedReader
                (new InputStreamReader(socket.getInputStream()));
	
            String outputLine; String inputLine;
            
            // Initialize bank protocol
            
            BankProtocol protocol = new BankProtocol(clients);
            
            // Two fist inputs are null, server will begin with sending welcome message
            // And ask for credit card number
            
            outputLine = protocol.processInput(null);
            out.println(outputLine);
            outputLine = protocol.processInput(null);
            out.println(outputLine);
            
            // Processes every user input from the socket
            
            while ((inputLine = in.readLine()) != null) {
                outputLine = protocol.processInput(inputLine);
                write10Bytes(outputLine, out);
                if (outputLine.equals("Bye.") || outputLine.equals("Hej da.") )
                    break;
            }
           
            // When user is done
            socket.close();
            in.close();
            out.close();
        
        }catch (IOException e){
            e.printStackTrace();
        }
    
    }
}
