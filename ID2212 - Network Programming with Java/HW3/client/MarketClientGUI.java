package se.kth.id2212.client;

import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.rmi.RemoteException;
import java.util.Collection;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import se.kth.id2212.bank.RejectedException;
import se.kth.id2212.marketplace.Activity;
import se.kth.id2212.marketplace.DbException;
import se.kth.id2212.marketplace.MarketPlace;
import se.kth.id2212.marketplace.Item;

/**
 * A class that builds the market client user interface with associated
 * components, behaviour and interaction with the marketplace.
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 */
@SuppressWarnings("serial")
public class MarketClientGUI extends JPanel {
    
	private JTextField regNameField;
	private JTextField regPasswordField;
	private JButton regButton;
	
	private JButton loginButton;
	
	private JTextField itemField;
	private JTextField priceField;
	private JButton sellButton;
	
	private DefaultListModel<String> itemListModel;
	private JButton updateButton;
	private JButton buyButton;
	
	private JTextField wishItemField;
	private JTextField wishPriceField;
	private JButton wishButton;
	
	private JButton updateActivityButton;
	
	private MarketPlace marketobj;
	private MarketClientImpl client;

	/**
	 * Creates a new instance and builds as well as initializes the GUI.
	 * 
	 * @param marketplaceClient
	 *            the client this GUI is associated with.
	 */
    public MarketClientGUI(MarketPlace marketobj, MarketClientImpl client) {
    	this.marketobj = marketobj;
    	this.client = client;
        buildGui();
        initGui();
    }

	/**
	 * Builds the GUI with the different panel components defined below and
	 * uses box layout to position them vertically.
	 */
    private void buildGui() {
        Box box = Box.createVerticalBox(); 
        box.add(registerPanel());
        box.add(loginPanel());
        box.add(sellPanel());
        box.add(itemsPanel());
        box.add(wishPanel());
        box.add(activityPanel());
        add(box);
    }

	/**
	 * Initializes the GUI to make fields and buttons that require a logged in
	 * user unavailable.
	 */
    private void initGui() {
    	itemField.setEnabled(false);
    	priceField.setEnabled(false);
    	sellButton.setEnabled(false);
    	buyButton.setEnabled(false);
    	updateActivityButton.setEnabled(false);
    }

    /**
     * Enables all the fields that require a logged in user.
     */
    private void userLoggedin() {
    	itemField.setEnabled(true);
    	priceField.setEnabled(true);
    	sellButton.setEnabled(true);
    	buyButton.setEnabled(true);
    	updateActivityButton.setEnabled(true);
    	
    	regNameField.setEnabled(false);
    	regPasswordField.setEnabled(false);
    	regButton.setEnabled(false);
    }

	/**
	 * Creates a registration panel for user registration.
	 * 
	 * @return the created panel.
	 */
    private Component registerPanel() {
    	JPanel registerPanel = new JPanel();
    	registerPanel.setBorder(new TitledBorder(new EtchedBorder(), "Registration"));

		JLabel usernameLabel = new JLabel("Username:");
		regNameField = new JTextField(10);
		usernameLabel.setLabelFor(regNameField);
		registerPanel.add(usernameLabel);
		registerPanel.add(regNameField);
		
		JLabel passwordLabel     = new JLabel("Password:");
		regPasswordField = new JTextField(10);
		usernameLabel.setLabelFor(regPasswordField);
		registerPanel.add(passwordLabel);
		registerPanel.add(regPasswordField);
		
		regButton = new JButton("Register");
		registerPanel.add(regButton);
		
		Action action = new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				String username = regNameField.getText();
				String password = regPasswordField.getText();
				
				if (username.equals("") ||
					password.equals(""))
					showErrorPane("No username or password selected");
				else {
					if (password.length() < 9)
						showErrorPane("Password too short, atleast 8 characters required");
					else {
						try {
							marketobj.registerClient(username,password);
						} catch (RemoteException e1) {
							showErrorPane(e1.getMessage());
						} catch (DbException e1) {
							showErrorPane(e1.getMessage());
						}
					}
				}
			}
		};
		
		regPasswordField.addActionListener(action);
		regButton.addActionListener(action);
		
		return registerPanel;
    }

	/**
	 * Creates a login panel for user credentials.
	 * 
	 * @return the created panel.
	 */
	private Component loginPanel() {
		JPanel loginPanel = new JPanel();
		loginPanel.setBorder(new TitledBorder(new EtchedBorder(), "Login"));

		JLabel usernameLabel = new JLabel("Username:");
		JTextField nameField = new JTextField(10);
		usernameLabel.setLabelFor(nameField);
		loginPanel.add(usernameLabel);
		loginPanel.add(nameField);
		
		JLabel passwordLabel = new JLabel("Password:");
		JTextField passwordField = new JTextField(10);
		passwordLabel.setLabelFor(passwordField);
		loginPanel.add(passwordLabel);
		loginPanel.add(passwordField);

		loginButton = new JButton("Login");
		loginPanel.add(loginButton);
		
		Action action = new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				String username = nameField.getText();
				String password = passwordField.getText();
				
				if (username.equals("") ||
					password.equals(""))
					showErrorPane("Username or password not selected");
				else {
					client.setUsername(nameField.getText());
					try { 
						marketobj.loginClient(client, password);
						userLoggedin();
				    	loginButton.setEnabled(false);
				    	nameField.setEnabled(false);
				    	passwordField.setEnabled(false);
					} catch (RemoteException e1) {
						showErrorPane(e1.getMessage());
					} catch (DbException e1) {
						showErrorPane(e1.getMessage());
					}
				}
			}
		};
		
		nameField.addActionListener(action);
		loginButton.addActionListener(action);
		
		return loginPanel;
	}

	/**
	 * Creates a sell panel for selling items.
	 * 
	 * @return the created panel.
	 */
	private Component sellPanel() {
		JPanel sellPanel = new JPanel();
		sellPanel.setBorder(new TitledBorder(new EtchedBorder(), "Sell"));

		JLabel itemLabel = new JLabel("Item:");
		itemField = new JTextField(10);
		itemLabel.setLabelFor(itemField);
		sellPanel.add(itemLabel);
		sellPanel.add(itemField);

		JLabel priceLabel = new JLabel("Price:");
		priceField = new JTextField(10);
		priceLabel.setLabelFor(priceField);
		sellPanel.add(priceLabel);
		sellPanel.add(priceField);

		sellButton = new JButton("Sell");
		sellPanel.add(sellButton);
		
		Action action = new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				String item  = itemField.getText();
				String price = priceField.getText(); 
				
				if (item.equals("") ||
					price.equals(""))
					showErrorPane("Item of price not choosen.");
				else {
					try {
						int priceInt = Integer.parseInt(priceField.getText());
						marketobj.sell(item, priceInt, client);
						itemField.setText("");
						priceField.setText("");
						updateItemList();
					} catch (RemoteException e1) {
						showErrorPane(e1.getMessage());
					} catch (NumberFormatException e1) {
						showErrorPane("Choosen price is not numeric");
					} catch (DbException e1) {
						showErrorPane(e1.getMessage());
					}
				}				
			}
		};
		
		priceField.addActionListener(action);
		sellButton.addActionListener(action);

		return sellPanel;
	}

	/**
	 * Creates an item panel for available items and the ability to buy any of
	 * them.
	 * 
	 * @return the created panel.
	 */
	private Component itemsPanel() {
		JPanel itemPanel = new JPanel();
		itemPanel.setBorder(new TitledBorder(new EtchedBorder(), "Buy"));
		
		itemListModel = new DefaultListModel<>();
		JList<String> itemList = new JList<>(itemListModel);
		itemList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		itemList.setSelectedIndex(0);
		itemList.setVisibleRowCount(7);

		JScrollPane itemListScrollPane = new JScrollPane(itemList);
		
		itemPanel.add(itemListScrollPane);
		
		updateButton = new JButton("Update");
		itemPanel.add(updateButton);
		
		updateButton.addActionListener(e -> {
			updateItemList();
		});

		buyButton = new JButton("Buy");
		itemPanel.add(buyButton);

		buyButton.addActionListener(e -> {
			try {
				String[] item = itemList.getSelectedValue().split("\\040-\\040");
				if (item.length < 2)
					showErrorPane("No product choosen");
				else {
					if (item[2].equals(client.getUsername()))
						showErrorPane("Can't buy your own items");
					else {
						String name = item[0];
						double price = Double.parseDouble(item[1]);
						marketobj.buy(name, price, client);
					}
				}
			} catch (RemoteException e1) {
				showErrorPane(e1.getMessage());
			} catch (DbException e1) {
				showErrorPane(e1.getMessage());
			} catch (RejectedException e1) {
				showErrorPane(e1.getMessage());
			}
			updateItemList();
		});
		
		return itemPanel;
	}

	/**
	 * Creates a wish panel for the ability to wish for items at a specific
	 * price.
	 * 
	 * @return the created panel.
	 */
	private Component wishPanel() {
		JPanel wishPanel = new JPanel();
		wishPanel.setBorder(new TitledBorder(new EtchedBorder(), "Wish"));
		
		JLabel itemLabel = new JLabel("Item:");
		wishItemField = new JTextField(10);
		itemLabel.setLabelFor(wishItemField);
		wishPanel.add(itemLabel);
		wishPanel.add(wishItemField);

		JLabel priceLabel = new JLabel("Price:");
		wishPriceField = new JTextField(10);
		priceLabel.setLabelFor(wishPriceField);
		wishPanel.add(priceLabel);
		wishPanel.add(wishPriceField);

		wishButton = new JButton("Wish");
		wishPanel.add(wishButton);
		
		wishButton.addActionListener(e -> {
			try {
				String item = wishItemField.getText();
				int price = Integer.parseInt(wishPriceField.getText());
				marketobj.wish(item, price, client);
				wishItemField.setText("");
				wishPriceField.setText("");
			} catch (RemoteException e1) {
				showErrorPane(e1.getMessage());
			} catch (NumberFormatException e2) {
				showErrorPane("Choosen price is not numeric");
			}
		});
		
		return wishPanel;
	}
	
	/**
	 * Creates a activity panel for the ability to view user statistics.
	 * 
	 * @return the created panel.
	 */
	private Component activityPanel() {
		JPanel activityPanel = new JPanel();
		activityPanel.setBorder(new TitledBorder(new EtchedBorder(), "User activity"));
		activityPanel.setLayout(new GridLayout(3, 1));

		updateActivityButton = new JButton("Update");
		activityPanel.add(updateActivityButton);
		
		JLabel totalSoldLabel = new JLabel("Number of sold items:");
		activityPanel.add(totalSoldLabel);
		JLabel totalBoughtLabel = new JLabel("Number of bought items:");
		activityPanel.add(totalBoughtLabel);
		
		updateActivityButton.addActionListener(e -> {
			try {
				Activity a = marketobj.getActivity(client);
				totalSoldLabel.setText("Number of sold items: " + a.getTotSold());
				totalBoughtLabel.setText("Number of bought items: " + a.getTotBought());
			} catch (RemoteException e1) {
				showErrorPane(e1.getMessage());
			} catch (NumberFormatException e2) {
				showErrorPane("Choosen price is not numeric");
			} catch (DbException e1) {
				showErrorPane(e1.getMessage());
			}
		});
		
		return activityPanel;
	}

	/**
	 * Updates the list of items.
	 */
	private void updateItemList() {
		try {
			Collection<Item> test = marketobj.inspect();
			itemListModel.clear();
			for(Item i : test) 
				itemListModel.addElement(i.getName() + " - " + i.getPrice() + " - " + i.getOwner());
		} catch (RemoteException e1) {
			showErrorPane(e1.getMessage());
		} catch (DbException e) {
			showErrorPane(e.getMessage());
		}
	}

	/**
	 * Shows a error message.
	 * 
	 * @param msg
	 *            the message to show.
	 */
	private void showErrorPane(String msg) {
		JOptionPane.showMessageDialog(
			this, 
			"<html><body><p style='width: 200px;'>"+ msg +"</p></body></html>",  
			"Error", JOptionPane.ERROR_MESSAGE
		);
		
	}

	/**
	 * Receives messages from the server and displays them.
	 * 
	 * @param msg
	 *            the message to display.
	 */
	public void reciveNotification(String msg) {
		JOptionPane.showMessageDialog(this, msg);
	}
}