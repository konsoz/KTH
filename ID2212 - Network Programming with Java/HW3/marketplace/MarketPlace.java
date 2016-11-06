package se.kth.id2212.marketplace;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;

import se.kth.id2212.bank.RejectedException;
import se.kth.id2212.client.MarketClient;

/**
 * 
 * This interface provides clients to do several things on a marketplace,
 * to sell, buy or to wish items.
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 */

public interface MarketPlace extends Remote {
	
	public void sell(String item, double price, MarketClient owner) throws RemoteException, DbException;
	
	public void buy(String item, double price, MarketClient buyer) throws RemoteException, RejectedException, DbException;
	
	public List<Item> inspect() throws RemoteException, DbException;
	
	public void registerClient(String username, String password) throws RemoteException, DbException;
	
	public void loginClient(MarketClient client, String password) throws RemoteException, DbException;
	
	public void logoutClient(MarketClient client) throws RemoteException;
	
	public void wish(String item, double price, MarketClient wisher) throws RemoteException;

	public Activity getActivity(MarketClient client) throws RemoteException, DbException;

	public void closeDBConnection() throws RemoteException, DbException;
	
}
