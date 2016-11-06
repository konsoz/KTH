package se.kth.id2212.marketplace;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.List;

import se.kth.id2212.bank.Account;
import se.kth.id2212.bank.Bank;
import se.kth.id2212.bank.RejectedException;
import se.kth.id2212.client.MarketClient;

/**
 * Marketplace implementation that handles several clients and a database
 * containing the registered users and marketplace products.
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 */
@SuppressWarnings("serial")
public class MarketPlaceImpl extends UnicastRemoteObject implements MarketPlace {
	private final Bank bank;
	private final Database db;
	
	private final List<MarketClient> clients;
	private final List<Item> wishes;

	/**
	 * Creates a marketplace with a connection to a bank.
	 * 
	 * @param bank
	 *            a connection to a bank.
	 * @throws RemoteException
	 *             if there is a problem with the remote object.
	 */
	public MarketPlaceImpl(Bank bank) throws RemoteException {
		this.bank = bank;
		this.db = new Database();
		
		wishes = new ArrayList<>();
		clients = new ArrayList<>();
		
	}

	/**
	 * Sell an item on the marketplace
	 * 
	 * @param name
	 *            item name.
	 * @param price
	 *            item price.
	 * @param owner
	 *            item owner.
	 * @throws RemoteException
	 *             if there is a problem with the remote object.
	 * @throws DbException
	 *             if there is a problem with the database.
	 */
	@Override
	public void sell(String name, double price, MarketClient owner) throws RemoteException, DbException {
		for(Item wish : wishes) {
			if (wish.getName().equalsIgnoreCase(name) &&
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
		
		db.insertItem(owner.getUsername(), name, price);
	}

	/**
	 * Buys a item on the store.
	 * 
	 * @param name
	 *            item name.
	 * @param price
	 *            item price.
	 * @param buyer
	 *            the person who wants to buy the item.
	 * @throws RemoteException
	 *             if there is a problem with the remote object.
	 * @throws RejectedException
	 *             if there is a problem with the bank.
	 * @throws DbException
	 *             if there is a problem with the database.
	 */
	@Override
	public void buy(String name, double price, MarketClient buyer) throws RemoteException, RejectedException, DbException {
		// Check availability
		Item itemToBuy = db.getItem(name, price);
		
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
				db.incrementSold(c.getUsername());
			}
		}
		db.incrementBought(buyer.getUsername());
		
		db.deleteItem(name, price);
	}

	/**
	 * Gets the items available on the marketplace.
	 * 
	 * @returns a list of the items.
	 * @throws RemoteException
	 *             if there is a problem with the remote object.
	 * @throws DbException
	 *             if there is a problem with the database.
	 */
	@Override
	public List<Item> inspect() throws RemoteException, DbException {
		return db.getItems();
	}

	/**
	 * Register a new user.
	 * 
	 * @param username
	 *            the users username.
	 * @param password
	 *            the users password.
	 * @throws RemoteException
	 *             if there is a problem with the remote object.
	 * @throws DbException
	 *             if there is a problem with the database.
	 */
	@Override
	public void registerClient(String username, String password) throws RemoteException, DbException {
		db.register(username, password);
	}
	
	/**
	 * Login a client.
	 * 
	 * @param client
	 *            connection to the client.
	 * @param password
	 *            the clients password.
	 * @throws RemoteException
	 *             if there is a problem with the remote object.
	 * @throws DbException
	 *             if there is a problem with the database.
	 */
	@Override
	public void loginClient(MarketClient client, String password) throws RemoteException, DbException {
		db.login(client.getUsername(), password);
		clients.add(client);
	}
	
	/**
	 * Logout a client.
	 * 
	 * @param client
	 *            connection to the client.
	 * @throws RemoteException
	 *             if there is a problem with the remote object.
	 */
	@Override
	public void logoutClient(MarketClient client) throws RemoteException {
		clients.remove(client);
	}

	/**
	 * Add a wish for a item at a specific price.
	 * 
	 * @param name
	 *            name of the item.
	 * @param password
	 *            price of the item.
	 * @param wisher
	 *            the connection to the client who is wishing for the product.
	 * @throws RemoteException
	 *             if there is a problem with the remote object.
	 */
	@Override
	public void wish(String name, double price, MarketClient wisher) throws RemoteException {
		wishes.add(new ItemImpl(name, wisher.getUsername(), price));
	}

	/**
	 * Get the activity statistics of a client.
	 * 
	 * @param client
	 *            connection to the client.
	 * @throws RemoteException
	 *             if there is a problem with the remote object.
	 * @throws DbException
	 *             if there is a problem with the database.
	 */
	@Override
	public Activity getActivity(MarketClient client) throws RemoteException, DbException {
		return db.getActivity(client.getUsername());
	}

	@Override
	public void closeDBConnection() throws DbException {
		db.closeConnection();
	}
	
}
