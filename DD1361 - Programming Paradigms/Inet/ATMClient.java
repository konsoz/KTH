import java.io.*;   
import java.net.*;  
import java.util.Scanner;

/**
   @author Snilledata
*/
public class ATMClient {
    private static int connectionPort = 8989;
    
    public static void main(String[] args) throws IOException {
        
        Socket ATMSocket = null;
        PrintWriter out = null;
        BufferedReader in = null;
        String adress = "";
        BufferedReader stdIn;

        
        stdIn = new BufferedReader(new InputStreamReader(System.in));
        
        try {
            adress = "127.0.0.1";
        } catch (ArrayIndexOutOfBoundsException e) {
            System.err.println("Missing argument ip-adress");
            System.exit(1);
        }
        try {
            ATMSocket = new Socket(adress, connectionPort); 
            out = new PrintWriter(ATMSocket.getOutputStream(), true);
            in = new BufferedReader(new InputStreamReader
                                    (ATMSocket.getInputStream()));
           
        } catch (UnknownHostException e) {
            System.err.println("Unknown host: " +adress);
            System.exit(1);
        } catch (IOException e) {
            System.err.println("Couldn't open connection to " + adress);
            System.exit(1);
        }

        System.out.println("Contacting bank ... ");
        
        String output;
        int length;
        
        // Welcome message
   
        System.out.println(in.readLine()); 

        // Enter credit card message
       
        System.out.println(in.readLine());
        
       
        System.out.print("> ");
    
        String userInput;
        
      
        
       while ((userInput = stdIn.readLine()) != null) {
        	write10Bytes(userInput, out);
            output = in.readLine();
        	System.out.println(output);
            System.out.print("> ");
            if(output.equals("Bye.") || output.equals("Hej da.")) break; /** asså ... */
        }
        
        ATMSocket.close();
        in.close();
        out.close();
        stdIn.close();
    }
    
    private static void write10Bytes(String toSend, PrintWriter out) {
    	StringBuilder str = new StringBuilder(toSend);
    	while(str.length() > 5) {
    		String send = str.substring(0, 5);
    		out.print(send);
    		str.delete(0, 5);
    	}  		
    	out.println(str.toString());
    }
    
   
}   
