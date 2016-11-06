package se.kth.id2212.marketplace;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.List;

import se.kth.id2212.bank.Account;
import se.kth.id2212.bank.Bank;
import se.kth.id2212.bank.RejectedException;
import se.kth.id2212.client.MarketClient;

@SuppressWarnings("serial")
public class MarketPlaceImpl extends UnicastRemoteObject implements MarketPlace {
	private final Bank bank;
	private final List<MarketClient> clients;
	private final List<Item> onSaleItems;
	private final List<Item> wishes;
	
	public MarketPlaceImpl(Bank bank) throws RemoteException {
		this.bank = bank;
		
		wishes = new ArrayList<>();
		onSaleItems = new ArrayList<>();
		clients = new ArrayList<>();
		
	}

	@Override
	public void sell(String item, double price, MarketClient owner) throws RemoteException {
		for(Item wish : wishes) {
			if (wish.getName().equalsIgnoreCase(item) &&
				wish.getPrice() > price) {
				for(MarketClient c : clients) {
					if (c.getUsername().equals(wish.getOwner()))
						c.reciveNotification(
							"The product (" + wish.getName() + ") you wished for " + 
							"is now available for " + price + "kr."
						);
				}
			}
		}
		
		onSaleItems.add(new ItemImpl(item, owner.getUsername(), price));
	}

	@Override
	public void buy(String item, double price, MarketClient buyer) throws RemoteException, RejectedException, Exception {
		// Check availability
		Item itemToBuy = null;
		for(Item i : onSaleItems) {
			if (i.getName().equalsIgnoreCase(item) && 
				i.getPrice() == price) 
					itemToBuy = i;
		}
		if (itemToBuy == null)
			throw new Exception("Item not found");
		
		Account bankAcc = bank.getAccount(buyer.getUsername());
		bankAcc.withdraw((float)price);
		
		// Notify owner of the product

		for(MarketClient c : clients) {
			if (c.getUsername().equals(itemToBuy.getOwner())) {
				Account bankAccSeller = bank.getAccount(c.getUsername());
				bankAccSeller.deposit((float)itemToBuy.getPrice());
				c.reciveNotification(
					"Your item (" + itemToBuy.getName() + ", " + itemToBuy.getPrice() + "kr)" +
					" has been sold."
				);
			}
		}
		
		onSaleItems.remove(itemToBuy);
	}

	@Override
	public List<Item> inspect() throws RemoteException {
		return onSaleItems;
	}

	@Override
	public void registerClient(MarketClient client) throws RemoteException, Exception {
		if (bank.getAccount(client.getUsername()) == null) 
			throw new Exception("Username not valid");
		else 
			clients.add(client);
	}

	@Override
	public void unRegisterClient(MarketClient client) throws RemoteException {
		clients.remove(client);
	}

	@Override
	public void wish(String item, double price, MarketClient wisher)
			throws RemoteException {
		wishes.add(new ItemImpl(item, wisher.getUsername(), price));
	}

}
