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
	
	public void sell(String item, double price, MarketClient owner) throws RemoteException;
	
	public void buy(String item, double price, MarketClient buyer) throws RemoteException, RejectedException, Exception;
	
	public List<Item> inspect() throws RemoteException;
	
	public void registerClient(MarketClient client) throws RemoteException, Exception;
	
	public void unRegisterClient(MarketClient client) throws RemoteException;
	
	public void wish(String item, double price, MarketClient wisher) throws RemoteException;
	
}
