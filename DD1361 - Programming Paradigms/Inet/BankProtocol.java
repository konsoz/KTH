import java.net.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.ResourceBundle;
import java.util.stream.Stream;
import java.io.*;

/**
 * 
 * This class represents bank protocol with different states for a user.
 * 
 * @author konstantin
 *
 */

enum State {
	WAITING, SENT_WELCOME, ASKED_CREDIT_CARD, ASKED_PIN_CODE, 
	USER_ACTIVE, WITHDRAWL, DEPOSIT, ASK_FOR_SECRET_CODE_W, 
	ASK_FOR_SECRET_CODE_D, CHANGE_LANGUAGE;
}

public class BankProtocol {

	private List<Account> accs;

	// User info
	private int creditCard;
	private int pinCode;
	private int secretCode;

	private State state = State.WAITING;
	
	// Messages in swedish
	ResourceBundle messages  = ResourceBundle.getBundle("sv_SE");
	
	private Account acc;

	public BankProtocol(List<Account> accs) {
		this.accs = accs;
	}

	public String processInput(String theInput) {
        String theOutput = null;
        
        switch(state) {
		case WAITING:
            theOutput = readWelcomeMsg();		// Waiting for some user to connect
            state = State.SENT_WELCOME;
			break;
		case SENT_WELCOME:
            theOutput = messages.getString("enterCreditCard");    // Send welcome message
            state = State.ASKED_CREDIT_CARD;
			break;
		case ASKED_CREDIT_CARD:
    		creditCard = Integer.parseInt(theInput);  // Get the credit card number
            theOutput = messages.getString("enterPinCode");
            state = State.ASKED_PIN_CODE;
			break;
		case ASKED_PIN_CODE:
    		pinCode = Integer.parseInt(theInput);	 // Get the pin code
    		acc = validateUser(creditCard, pinCode); // Validate the user with pin and card number
    		if(acc == null) {
                theOutput = messages.getString("tryAgain") + "\n" + messages.getString("enterCreditCard");			// If some field is wrong
    			state = State.ASKED_PIN_CODE; 	/** borde de inte vara askedcreditcard eller askedpincode? */
    		} else {
    			theOutput = messages.getString("menu");
    			state = State.USER_ACTIVE;
    		}
			break;
		case USER_ACTIVE:				// User has logged in
        	int choice = Integer.parseInt(theInput);
        	switch(choice) {
			case 1:
				theOutput = messages.getString("currentBalance")
						+ Integer.toString(acc.getBalance()) + "\t"
						+ messages.getString("menu");
				state = State.USER_ACTIVE;
    			break;
        	case 2:
        		theOutput = messages.getString("enterSecretCode");
    			state = State.ASK_FOR_SECRET_CODE_W;
    			break;
        	case 3:
        		theOutput = messages.getString("enterSecretCode");
    			state = State.ASK_FOR_SECRET_CODE_D;
    			break;
        	case 4: 
        		theOutput = messages.getString("exit");
    			state = State.WAITING;
        		break;
        	case 5:
        		theOutput = messages.getString("languages");
    			state = State.CHANGE_LANGUAGE;
    			break;
        	}
			break;
		case ASK_FOR_SECRET_CODE_D:		// User was asked for secret code for deposit
        	secretCode = Integer.parseInt(theInput);
        	if(acc.isLegitSecretCode(secretCode)) {
        		theOutput = messages.getString("enterAmount");
        		state = State.DEPOSIT;
        	} else {
                theOutput = messages.getString("tryAgain");	
        		state = State.USER_ACTIVE;
        	}
			break;
		case ASK_FOR_SECRET_CODE_W:	// User was asked for secret code for withdraw
        	secretCode = Integer.parseInt(theInput);
        	if(acc.isLegitSecretCode(secretCode)) {
        		theOutput = messages.getString("enterAmount");
        		state = State.WITHDRAWL;
        	} else {
                theOutput = messages.getString("tryAgain");	
        		state = State.USER_ACTIVE;
        	}
			break;
		case WITHDRAWL:			// User are about to withdraw
	       	int amountW = Integer.parseInt(theInput);
	       	theOutput = "";
	       	try {
	       		Thread.sleep(5_000);
	       		acc.withdrow(amountW);
	       	} catch (Exception e) {
	       		theOutput = e.getMessage();
	       	}
	       	theOutput += messages.getString("menu");
	       	state = State.USER_ACTIVE;
			break;
		case DEPOSIT:				// User are about to deposit
        	int amountD = Integer.parseInt(theInput);
       	 	acc.deposit(amountD);
       	 	theOutput = messages.getString("menu");;
       	 	state = State.USER_ACTIVE; 
			break;
		case CHANGE_LANGUAGE:				// User wants to change the language
            int newLang = Integer.parseInt(theInput);
            if (newLang == 1) 
            	messages = ResourceBundle.getBundle("en_US");
            else if (newLang == 2) 
            	messages = ResourceBundle.getBundle("sv_SE");
        	theOutput = messages.getString("menu");
        	state = State.USER_ACTIVE;
			break;
        }
        return theOutput;
    }

	
	/**
	 * 
	 * This method will validate a user with a credit card number and a pin code
	 * 
	 * @param creditCard - credit card number
	 * @param pinCode - pin code
	 * @return Account for the user.
	 */
	
	private Account validateUser(int creditCard, int pinCode) {
		for (Account account : accs) {
			if (account.getCreditCard() == creditCard
					&& account.getSecretCode() == pinCode) {
				return account;
			}
		}
		return null;
	}
	
	public String readWelcomeMsg() {
		StringBuilder str = new StringBuilder();
        Path path = Paths.get("/home/konstantin/Documents/KTH/Programmeringsparadigm/inet/welcome.txt");
        try (Stream<String> lines = Files.lines(path)) {
            lines.forEach(s -> str.append(s));
        } catch (IOException ex) {
        	System.out.println(ex.getMessage());
        }
        return str.toString();
    }
}