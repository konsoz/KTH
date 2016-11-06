package se.kth.id2212.marketplace;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Interface for the activity DTO.
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 */
public interface Activity extends Remote {
	public int getTotSold() throws RemoteException;
	public int getTotBought() throws RemoteException;
}
