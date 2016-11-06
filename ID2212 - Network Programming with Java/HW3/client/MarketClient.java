package se.kth.id2212.client;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Marketplace interface for the marketplace client.
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 */
public interface MarketClient extends Remote {
	public void reciveNotification(String msg) throws RemoteException;
	public String getUsername() throws RemoteException;
}
