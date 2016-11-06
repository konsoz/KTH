package se.kth.id2212.marketplace;

import java.rmi.RemoteException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public class Database {
	
    /** DB connection variable. */
    static protected Connection con;
    
    /** DB access variables. */
    private String URL = "jdbc:mysql://localhost/HW3";
    private String driver = "com.mysql.jdbc.Driver";
    private String userID = "root";
    private String password = "d4rk5t4r";
	
    /**
     * Establishes the connection to the database.
     */
    Database() {
        try {
            Class.forName(driver).newInstance();
            con = DriverManager.getConnection(URL, userID, password);
            con.setAutoCommit(true);
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(0);
        }
    }
    
    /**
	 * Closes the connection to the database.
	 * 
	 * @throws DbException
	 *             If a database error occurs.
	 */
    void closeConnection() throws DbException {
        try {
			con.close();
		} catch (SQLException e) {
			throw new DbException(e.getMessage());
		}
    }
	
	/**
	 * Registers a new client.
	 * 
	 * @param username client username.
	 * @param password client password.
	 * @throws DbException if the user already exist. 
	 */
	protected void register(String username, String password) throws DbException{
		String query = "INSERT INTO User (name,password)"
				+ " VALUES (?,?)";
		try {
			 PreparedStatement stmt = con.prepareStatement(query);
		        stmt.setString(1, username);
		        stmt.setString(2, password);
		        stmt.executeUpdate();
		        stmt.close();
		} catch(SQLException sqlException) {
			throw new DbException("User exists");
		}
	}
	
	/**
	 * Login the user by checking if the username and password exist.
	 * 
	 * @param username
	 *            the clients username.
	 * @param password
	 *            the clients password.
	 * @throws DbException
	 *             if the username or password is incorrect.
	 */
	protected void login(String username, String password) throws DbException {
		 String query = "SELECT * FROM User"
	     	 		+ " WHERE name=? AND password=?";
        
        try {
        	PreparedStatement stmt = con.prepareStatement(query);
            
            stmt.setString(1, username);
            stmt.setString(2, password);
            
            ResultSet rs = stmt.executeQuery();
            
            if (!rs.next() ) {
                throw new DbException("Wrong username or/and password");
            }
            
            stmt.close();
        } catch(SQLException sqlException) {
        	throw new DbException(sqlException.toString());
        }
    }

	/**
	 * Adds an item to the database.
	 * 
	 * @param owner
	 *            item owner.
	 * @param name
	 *            item name.
	 * @param price
	 *            item price.
	 * @throws DbException
	 *             if the item already exist.
	 */
	protected void insertItem(String owner, String name, double price) throws DbException{
		String query = "INSERT INTO Item (name,price,owner)"
				+ " VALUES (?,?,?)";
		try {
			 PreparedStatement stmt = con.prepareStatement(query);
		        stmt.setString(1, name);
		        stmt.setDouble(2, price);
		        stmt.setString(3, owner);
		        stmt.executeUpdate();
		        stmt.close();
		} catch(SQLException sqlException) {
			throw new DbException(sqlException.toString());
		}
	}
	
	/**
	 * Gets all the item in the database.
	 * 
	 * @return a list of the items.
	 * @throws DbException
	 *             if there is an error executing the query.
	 * @throws RemoteException
	 *             if there is an error with the remote object.
	 */
	protected List <Item> getItems() throws DbException, RemoteException {
        String query = "SELECT * FROM Item";
        List <Item> items = new ArrayList<>();
        try {
        	PreparedStatement stmt = con.prepareStatement(query);   
            ResultSet rs = stmt.executeQuery();
            
            while (rs.next() ) {
                String itemName = rs.getString("name");
                double itemPrice = rs.getDouble("price");
                String owner = rs.getString("owner");
                items.add(new ItemImpl(itemName,owner, itemPrice)); 
            }
            
            stmt.close();
        } catch(SQLException sqlException) {
        	throw new DbException(sqlException.toString());
        }
        return items;
    }
	
	/**
	 * Gets a specific item from the database.
	 * 
	 * @param name
	 *            the name of the item.
	 * @param price
	 *            the price of the item.
	 * @return the item.
	 * @throws DbException
	 *             if there is an error executing the query.
	 * @throws RemoteException
	 *             if there is an error with the remote object.
	 */
	protected Item getItem(String name, double price) throws DbException, RemoteException {
		String query = "SELECT * FROM Item WHERE name=? AND price=?";
		try {
			PreparedStatement stmt = con.prepareStatement(query);
			stmt.setString(1,name);
			stmt.setDouble(2,price);
			
			ResultSet rs = stmt.executeQuery();
            
			if (!rs.next() ) {
                throw new DbException("Item not found");
            }
			
			Item i = new ItemImpl(
				rs.getString("name"),
				rs.getString("owner"),
				rs.getDouble("price"));
			
            stmt.close();
            
            return i;
		} catch(SQLException sqlException) {
			throw new DbException(sqlException.toString());
		}
	}
	
	/**
	 * Removes a specific item from the database.
	 * 
	 * @param name item name.
	 * @param price item price.
	 * @throws DbException
	 *             if there is an error executing the query.
	 */
	protected void deleteItem(String name, double price) throws DbException{
		String query = "DELETE FROM Item WHERE name=? AND price=?";
		try {
			 PreparedStatement stmt = con.prepareStatement(query);
		        stmt.setString(1, name);
		        stmt.setDouble(2, price);
		        stmt.executeUpdate();
		        stmt.close();
		} catch(SQLException sqlException) {
			throw new DbException(sqlException.toString());
		}
	}
	
	/**
	 * Gets the activity information of a client.
	 * 
	 * @param username
	 *            the client username.
	 * @return a activity object containing the information.
	 * @throws DbException
	 *             if there is an error executing the query.
	 * @throws RemoteException
	 *             if there is an error with the remote object.
	 */
	protected Activity getActivity(String username) throws DbException, RemoteException {
        String query = "SELECT totSold, totBought FROM User"
        		+ " WHERE name=?";
        Activity activity = null;
        try {
        	PreparedStatement stmt = con.prepareStatement(query);   
        	stmt.setString(1, username);
            ResultSet rs = stmt.executeQuery();
            
            while (rs.next() ) {
            	int totSold = rs.getInt("totSold");
            	int totBought = rs.getInt("totBought");
                activity = new ActivityImpl(totSold, totBought); 
            }
            
            stmt.close();
        } catch(SQLException sqlException) {
        	throw new DbException(sqlException.toString());
        }
        return activity;
    }

	/**
	 * Increment the number of items sold for a specific client.
	 * 
	 * @param username
	 *            the clients username.
	 * @throws DbException
	 *             if there is an error executing the query.
	 */
	protected void incrementSold(String username) throws DbException {
		String query = "UPDATE User SET totSold = totSold + 1"
				+ " WHERE name=?";
		try {
			 PreparedStatement stmt = con.prepareStatement(query);
		        stmt.setString(1, username);
		        stmt.executeUpdate();
		        stmt.close();
		} catch(SQLException sqlException) {
			throw new DbException(sqlException.toString());
		}
	}

	/**
	 * Increment the number of items bought for a specific client.
	 * 
	 * @param username
	 *            the clients username.
	 * @throws DbException
	 *             if there is an error executing the query.
	 */
	protected void incrementBought(String username) throws DbException {
		String query = "UPDATE User SET totBought = totBought + 1"
				+ " WHERE name=?";
		try {
			 PreparedStatement stmt = con.prepareStatement(query);
		        stmt.setString(1, username);
		        stmt.executeUpdate();
		        stmt.close();
		} catch(SQLException sqlException) {
			throw new DbException(sqlException.toString());
		}
	}
}
