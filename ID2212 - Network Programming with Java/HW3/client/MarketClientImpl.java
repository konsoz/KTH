package se.kth.id2212.client;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.UnicastRemoteObject;

import javax.swing.JFrame;

import se.kth.id2212.marketplace.MarketPlace;

/**
 * Implementation of the client interface which handles the connection to the
 * marketplace.
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 */
@SuppressWarnings("serial")
public class MarketClientImpl extends UnicastRemoteObject implements MarketClient {
	private final static String DEFAULT_SHOP_NAME = "Inet";
	
	private MarketClientGUI gui;
	private String username;
	private MarketPlace marketobj = null;

	/**
	 * Creates a marketplace client with the supplied marketplace name.
	 * 
	 * @param marketplaceName
	 *            the name of the marketplace.
	 * @throws RemoteException
	 *             if there is any issue with the rmi connection.
	 */
    public MarketClientImpl(String marketplaceName) throws RemoteException {
    	try {
            try {
                LocateRegistry.getRegistry(1099).list();
            } catch (RemoteException e) {
                LocateRegistry.createRegistry(1099);
            }
            marketobj = (MarketPlace) Naming.lookup(marketplaceName);
        } catch (Exception e) {
            System.out.println("The runtime failed: " + e.getMessage());
            System.exit(0);
        }
        System.out.println("Connected to marketplace: " + marketplaceName);

        JFrame frame = new JFrame("Market Place is the place where you want to be");
        gui = new MarketClientGUI(marketobj,this);
        frame.setContentPane(gui);
        frame.pack();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    	MarketClient client = this;
        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
            	try {
					marketobj.logoutClient(client);
					marketobj.closeDBConnection();
				} catch (Exception e1) {
					e1.printStackTrace();
				}
                System.exit(0);
            }
        });
        frame.setVisible(true);
    }

    public MarketClientImpl() throws RemoteException {
        this(DEFAULT_SHOP_NAME);
    }
    
	/**
	 * Gets the notifications from the server and forwards them to the GUI.
	 */
	@Override
	public void reciveNotification(String msg) throws RemoteException {
		gui.reciveNotification(msg);
	}

	@Override
	public String getUsername() throws RemoteException {
		return username;
	}
	
	public void setUsername(String username){
		this.username = username;
	}
    
    public static void main(String[] args) throws RemoteException {
        String marketplaceName;
        if (args.length > 0) {
            marketplaceName = args[0];
            new MarketClientImpl(marketplaceName);
        } else {
            new MarketClientImpl();
        }
    }
    
}
