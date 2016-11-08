import java.util.List;

/**
 * 
 * A DTO for an account for a user. Provides simple access to user fields and operations.s
 * 
 * @author konstantin
 *
 */

public class Account {
	private final int creditCard;
	private final int secretCode;
	private int balance;
	private final List <Integer> codes;
	private final String name;
	
	public Account(int creditCard, int secretCode, int balance, List <Integer> codes, String name) {
		this.creditCard = creditCard;
		this.secretCode = secretCode;
		this.setBalance(balance);
		this.codes = codes;
		this.name = name;
	}
	
	
	public void deposit(int amount) {
		setBalance(getBalance() + amount);
	}
	

	public synchronized void withdrow(int amount) throws Exception {
		if(amount > getBalance()) {
			throw new Exception("Amount withdrawn larger then balance!");
		}
		setBalance(getBalance() - amount);
	}


	public int getCreditCard() {
		return creditCard;
	}


	public int getSecretCode() {
		return secretCode;
	}


	public int getBalance() {
		return balance;
	}


	public void setBalance(int balance) {
		this.balance = balance;
	}


	public List <Integer> getCodes() {
		return codes;
	}


	public String getName() {
		return name;
	}
	
	public boolean isLegitSecretCode(int code){
		for (Integer c : codes) {
			if (c == code) return true;
		}
		return false;
	}
}