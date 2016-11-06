package se.kth.id2212.marketplace;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;

import se.kth.id2212.bank.Bank;

public class MarketplaceServer {
	
	private static final String USAGE = "java marketplace.Server <bank_rmi_url> <marketplace_rmi_url>";
	private Bank bankobj;
	
	private final static String DEFAULTBANK = "Nordea";
	private final static String DEFAULTSHOP = "Inet";
	
	public MarketplaceServer(String marketPlaceName, String bankName) {
		try {
			// Get the bank object from RMI registry
			try {
				try {
					LocateRegistry.getRegistry(1099).list();
				} catch (RemoteException e) {
					LocateRegistry.createRegistry(1099);
				}
				bankobj = (Bank) Naming.lookup(bankName);
			} catch (Exception e) {
				System.out.println("The runtime failed: " + e.getMessage());
				System.exit(0);
			}

			// Create market place

			MarketPlace marketPlace = new MarketPlaceImpl(bankobj);
			
			// Register the newly created object at rmiregistry.
			
			try {
				LocateRegistry.getRegistry(1099).list();
			} catch (RemoteException e) {
				LocateRegistry.createRegistry(1099);
			}
			Naming.rebind(marketPlaceName, marketPlace);
            System.out.println(marketPlace + " is ready.");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static void main(String[] args) {
		
		 	if (args.length > 2 || (args.length > 0 && args[0].equalsIgnoreCase("-h"))) {
	            System.out.println(USAGE);
	            System.exit(1);
	        }

		 	
	        String bankName;
	        String marketPlaceName;
	        
	        if (args.length > 0) {
	            bankName = args[0];
	            marketPlaceName = args[1];
	        } else {
	            bankName = DEFAULTBANK;
	            marketPlaceName = DEFAULTSHOP;
	        }
	        
	        new MarketplaceServer(marketPlaceName, bankName);
	}
}
