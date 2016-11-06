package se.kth.id2212.ex4.shop.model;

import java.io.Serializable;
import java.util.HashMap;
import javax.persistence.Entity;
import javax.persistence.Id;

/**
 * Persistent entity representation of a shop user.
 */
@Entity
public class ShopUser implements Serializable {
    @Id
    private String username;
    private String password;
    private boolean isAdmin = false;
    private boolean isBanned = false;
    private HashMap<Gnome,Integer> cart;
    
    /**
     * Creates a new instance of a shop user.
     */
    public ShopUser() {}
    
    /**
     * Creates a new instance of a shop user with the suplied arguments.
     * 
     * @param username the new users username.
     * @param password the new users password.
     */
    public ShopUser(String username, String password) {
        this.username = username;
        this.password = password;
        cart = new HashMap<>();
    }
    
    /**
     * Authenticates a user.
     * 
     * @param password the password to test.
     * @return <code>true</code> if the the authentification succedes and
     * <code>false</code> of it fails.
     */
    public boolean authenticate(String password) {
        return (this.password.equals(password));
    }
    
    /**
     * Gets the users username.
     * 
     * @return the username.
     */
    public String getUsername() {
        return username;
    }
    
    /**
     * Get wheter of not the user is an admin.
     * 
     * @return <code>true</code> if the user is an admin otherwise
     * <code>false</code>.
     */
    public boolean getIsAdmin() {
        return isAdmin;
    }
    
    /**
     * Get wheter of not the user is banned.
     * 
     * @return <code>true</code> if the user is banned otherwise
     * <code>false</code>.
     */
    public boolean getIsBanned() {
        return isBanned;
    }
    
    /**
     * Sets wheter of not the user is banned.
     * 
     * @param isBanned the new banned state.
     */
    public void setIsBanned(boolean isBanned) {
        this.isBanned = isBanned;
    }
    
    /**
     * Remove a gnome from the users shopping cart.
     * 
     * @param gnome the gnome to remove.
     */
    public void removeFromCart(Gnome gnome) {
        cart.remove(gnome);
    }
    
    /**
     * Add a gnome and the quantity of it to the users shopping cart.
     * 
     * @param gnome the gnome to add.
     * @param amount the quantity.
     */
    public void addToCart(Gnome gnome, int amount) {
        int original = 0;
        if (cart.containsKey(gnome))
            original = cart.get(gnome);
        cart.put(gnome, amount+original);
    }
    
    /**
     * Gets the users shopping cart.
     * 
     * @return the shopping cart.
     */
    public HashMap<Gnome, Integer> getCart() {
        return cart;
    }
    
    /** 
     * Clears the users shopping cart.
     */
    public void emptyCart() {
        cart.clear();
    }
    
    /**
     * Gets the string representation of the object.
     * 
     * @return the string representation.
     */
    @Override
    public String toString() {
        return getUsername();
    }
}
