package se.kth.id2212.marketplace;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

/**
 * Item implementation for keeping information on an item.
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 */
@SuppressWarnings("serial")
public class ItemImpl extends UnicastRemoteObject implements Item {
	
	private final String name;
	private double price;
	private final String owner;
	
	
	public ItemImpl(String name, String owner, double price) throws RemoteException{
		this.name = name;
		this.owner = owner;
		this.price = price;
	}


	@Override
	public String getOwner() throws RemoteException {
		return owner;
	}


	@Override
	public String getName() throws RemoteException {
		return name;
	}


	@Override
	public double getPrice() throws RemoteException {
		return price;
	}

	
	
	
}
