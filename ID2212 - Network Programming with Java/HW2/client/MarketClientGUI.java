package se.kth.id2212.client;

import java.awt.Component;
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
    
	private JButton useButton;
	
	private JTextField itemField;
	private JTextField priceField;
	private JButton sellButton;
	
	private DefaultListModel<String> itemListModel;
	private JButton updateButton;
	private JButton buyButton;
	
	private JTextField wishItemField;
	private JTextField wishPriceField;
	private JButton wishButton;
	
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
	 * s Builds the GUI with the different panel components defined below and
	 * uses box layout to position them vertically.
	 */
	private void buildGui() {
        Box box = Box.createVerticalBox(); 
        box.add(namePanel());
        box.add(sellPanel());
        box.add(itemsPanel());
        box.add(wishPanel());
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
    }
    
    /**
     * Enables all the fields that require a logged in user.
     */
    private void enableSellAndBuy() {
    	itemField.setEnabled(true);
    	priceField.setEnabled(true);
    	sellButton.setEnabled(true);
    	buyButton.setEnabled(true);
    }

	/**
	 * Creates a login panel for user credentials.
	 * 
	 * @return the created panel.
	 */
	private Component namePanel() {
		JPanel namePanel = new JPanel();
		namePanel.setBorder(new TitledBorder(new EtchedBorder(), "Your username"));

		JLabel usernameLabel = new JLabel("Username:");
		JTextField nameField = new JTextField(10);
		usernameLabel.setLabelFor(nameField);
		namePanel.add(usernameLabel);
		namePanel.add(nameField);

		useButton = new JButton("Use");
		namePanel.add(useButton);
		
		Action action = new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				if (nameField.getText() == "") 
					showErrorPane("No username choosen");
				else {
					client.setUsername(nameField.getText());
					try {
						marketobj.registerClient(client);
					} catch (Exception e1) {
						showErrorPane(e1.getMessage());
					}
					enableSellAndBuy();
			    	useButton.setEnabled(false);
			    	nameField.setEnabled(false);
				}
			}
		};
		
		nameField.addActionListener(action);
		useButton.addActionListener(action);
		
		return namePanel;
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

		sellButton.addActionListener(e -> {
			if (itemField.getText() == "" ||
				priceField.getText() == "")
				showErrorPane("Item of price not choosen.");
			else {
				try {
					String item = itemField.getText();
					int price = Integer.parseInt(priceField.getText());
					marketobj.sell(item, price, client);
					itemField.setText("");
					priceField.setText("");
					updateItemList();
				} catch (RemoteException e1) {
					showErrorPane(e1.getMessage());
				} catch (NumberFormatException e2) {
					showErrorPane("Choosen price is not numeric");
				}
			}
		});

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
			} catch (Exception e1) {
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
			this, msg, "Error", JOptionPane.ERROR_MESSAGE
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