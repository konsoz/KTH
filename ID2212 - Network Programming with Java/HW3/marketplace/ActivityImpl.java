package se.kth.id2212.marketplace;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

/**
 * DTO for user activity to be sent to the user. 
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 */
@SuppressWarnings("serial")
public class ActivityImpl extends UnicastRemoteObject implements Activity {
	private final int totSold;
	private final int totBought;
	
	protected ActivityImpl(int totSold, int totBought) throws RemoteException{
		this.totSold = totSold;
		this.totBought = totBought;
	}

	public int getTotBought() {
		return totBought;
	}


	public int getTotSold() {
		return totSold;
	}
}
