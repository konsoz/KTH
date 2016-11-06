package se.kth.id2212.marketplace;

import java.rmi.*;

/**
 * Iterface for an item.
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 */
public interface Item extends Remote {
	public String getOwner() throws RemoteException;
	public String getName() throws RemoteException;
	public double getPrice() throws RemoteException;
}
