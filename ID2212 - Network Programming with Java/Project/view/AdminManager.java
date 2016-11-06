package se.kth.id2212.ex4.shop.view;

import java.io.Serializable;
import java.util.List;
import javax.ejb.EJB;
import javax.enterprise.context.SessionScoped;
import javax.inject.Named;
import se.kth.id2212.ex4.shop.controller.ShopController;
import se.kth.id2212.ex4.shop.model.ShopUser;
import se.kth.id2212.ex4.shop.model.UserNotFoundException;

/**
 * A manager for calls from the view concerning admin use cases.
 */
@Named("adminManager")
@SessionScoped
public class AdminManager implements Serializable {
    
    private static final long serialVersionUID = 16247164405L;
    @EJB
    private ShopController shopController;
    private Exception transactionFailure;
    private boolean isLoggedIn;
    private String newUsername;
    private String newPassword;
    private ShopUser currentUser;
    private String selectedUser;
    
    /**
     * Logout an admin.
     */
    public void logout() {
        setCurrentUser(null);
        setIsLoggedIn(false);
    }
    
    /**
     * Login an admin.
     */
    public void login() {
        try {
            transactionFailure = null;
            setCurrentUser(shopController.loginAdmin(newUsername, newPassword));
            setIsLoggedIn(true);
        } catch (Exception e) {
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
     * Bans a user.
     */
    public void banUser() {
        try {
            shopController.banUser(selectedUser);
        } catch (UserNotFoundException ex) {
            handleException(ex);
        }
    }
    
    /**
     * Getters and setters 
     */
    
    /**
     * Get all users of the webshop.
     * 
     * @return a list of the users.
     */
    public List<ShopUser> getUsers() {
        return shopController.getUsers();
    }
    
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

    public ShopUser getCurrentUser() {
        return currentUser;
    }

    public void setCurrentUser(ShopUser currentUser) {
        this.currentUser = currentUser;
    }

    public String getSelectedUser() {
        return selectedUser;
    }

    public void setSelectedUser(String selectedUser) {
        this.selectedUser = selectedUser;
    }

}
