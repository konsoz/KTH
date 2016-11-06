package se.kth.id2212.marketplace;

@SuppressWarnings("serial")
public class DbException extends Exception {

	public DbException(String reason) {
        super(reason);
    }
	
}
