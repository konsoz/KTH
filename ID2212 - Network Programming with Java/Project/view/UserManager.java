package se.kth.id2212.ex4.shop.view;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import javax.ejb.EJB;
import javax.enterprise.context.SessionScoped;
import javax.inject.Named;
import se.kth.id2212.ex4.shop.controller.ShopController;
import se.kth.id2212.ex4.shop.model.Gnome;
import se.kth.id2212.ex4.shop.model.GnomeAmountException;
import se.kth.id2212.ex4.shop.model.GnomeNotFoundException;
import se.kth.id2212.ex4.shop.model.ShopUser;
import se.kth.id2212.ex4.shop.model.UserExistException;
import se.kth.id2212.ex4.shop.model.UserLoginException;
import se.kth.id2212.ex4.shop.model.UserNotFoundException;

/**
 * A manager for calls from the view concerning users of the webshop.
 */
@Named("userManager")
@SessionScoped
public class UserManager implements Serializable {
    
    private static final long serialVersionUID = 16247164405L;
    @EJB
    private ShopController shopController;
    private boolean isLoggedIn;
    private String newUsername;
    private String newPassword;
    private boolean isRegistration;
    private Exception transactionFailure;
    private ShopUser currentUser;
    private Map<Gnome, String> quantities = new HashMap<>();
    
    /**
     * Logout a logged in user.
     */
    public void logout() {
        setCurrentUser(null);
        setIsLoggedIn(false);
    }
    
    /**
     * Login or register a user.
     */
    public void loginOrRegister() {
        try {
            transactionFailure = null;
            if (isRegistration) 
                setCurrentUser(shopController.createAccount(newUsername, newPassword));            
            else 
                setCurrentUser(shopController.login(newUsername, newPassword));
            setIsLoggedIn(true);
        } catch (UserExistException | UserLoginException e) {
            handleException(e);
        }
    }
    
    /**
     * Handle any exception.
     * 
     * @param e the exception to handle.
     */
    private void handleException(Exception e) {
        e.printStackTrace(System.err);
        transactionFailure = e;
    }
    
    /**
     * Gets if the latest transaction succeded.
     * 
     * @return <code>true</code> if the latest transaction succeeded, otherwise
     * <code>false</code>.
     */
    public boolean getSuccess() {
        return transactionFailure == null;
    }

    /**
     * Gets the latest thrown exception.
     * 
     * @return the exception.
     */
    public Exception getException() {
        return transactionFailure;
    }
    
    /**
     * Add a gnome to the cart.
     * 
     * @param gnome the gnome to add.
     */
    public void addToCart(Gnome gnome) {
        try {
            transactionFailure = null;
            int amount = Integer.parseInt(getQuantities().get(gnome));
            shopController.addToCart(currentUser, gnome, amount);
        } catch(UserNotFoundException | NumberFormatException e) {
            handleException(e);
        }
    }
    
    /**
     * Gets the shopping cart.
     * 
     * @return a mapping of gnomes to their quantity in the cart.
     */
    public HashMap<Gnome, Integer> getCart() {
        return shopController.getCart(currentUser);
    }
    
    /**
     * Pay for the contents of the shopping cart.
     */
    public void pay() {
        try {
            transactionFailure = null;
            shopController.pay(currentUser);
        } catch (UserNotFoundException | GnomeNotFoundException | GnomeAmountException ex) {
            handleException(ex);
        } 
    }
    
    /**
     * Getters and setters 
     */
    
    public boolean getIsLoggedIn() {
        return isLoggedIn;
    }

    public void setIsLoggedIn(boolean loggedIn) {
        isLoggedIn = loggedIn;
    }

    public String getNewUsername() {
        return newUsername;
    }

    public void setNewUsername(String newUsername) {
        this.newUsername = newUsername;
    }

    public String getNewPassword() {
        return newPassword;
    }

    public void setNewPassword(String newPassword) {
        this.newPassword = newPassword;
    }

    public boolean isIsRegistration() {
        return isRegistration;
    }

    public void setIsRegistration(boolean isRegistration) {
        this.isRegistration = isRegistration;
    }

    public ShopUser getCurrentUser() {
        return currentUser;
    }

    public void setCurrentUser(ShopUser currentUser) {
        this.currentUser = currentUser;
    }

    public Map<Gnome, String> getQuantities() {
        return quantities;
    }

    public void setQuantities(Map<Gnome, String> quantities) {
        this.quantities = quantities;
    }

}
