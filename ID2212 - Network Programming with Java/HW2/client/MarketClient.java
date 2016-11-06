package se.kth.id2212.client;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface MarketClient extends Remote {
	public void reciveNotification(String msg) throws RemoteException;
	public String getUsername() throws RemoteException;
}
