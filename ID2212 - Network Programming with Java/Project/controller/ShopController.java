package se.kth.id2212.ex4.shop.controller;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import se.kth.id2212.ex4.shop.model.Gnome;
import se.kth.id2212.ex4.shop.model.GnomeAmountException;
import se.kth.id2212.ex4.shop.model.GnomeNotFoundException;
import se.kth.id2212.ex4.shop.model.UserExistException;
import se.kth.id2212.ex4.shop.model.ShopUser;
import se.kth.id2212.ex4.shop.model.UserLoginException;
import se.kth.id2212.ex4.shop.model.UserNotFoundException;

/**
 * Application controller. All calls the to the model pass through here.
 */
@TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
@Stateless
public class ShopController {
    @PersistenceContext(unitName = "bankPU")
    private EntityManager em;
    
    /**
     * Register a new user.
     * 
     * @param username the username of the new user.
     * @param password the password of the new user.
     * @return the new user object.
     * @throws UserExistException if the username already is taken.
     */
    public ShopUser createAccount(String username, String password) throws UserExistException  {
        if (em.find(ShopUser.class, username) != null)
            throw new UserExistException("Account " + username + " already exist.");
        
        ShopUser newAcct = new ShopUser(username, password);
        em.persist(newAcct);
        
        return newAcct;
    }
    
    /**
     * Login a user.
     * 
     * @param username the users username.
     * @param password the users password.
     * @return the user object.
     * @throws UserLoginException if the username or password is incorrect or if
     * the user is banned.
     */
    public ShopUser login(String username, String password) throws UserLoginException {
        ShopUser user = em.find(ShopUser.class, username);
        if (user == null || !user.authenticate(password))
            throw new UserLoginException("Username or password incorrect.");
        if (user.getIsBanned())
            throw new UserLoginException("YOU'RE BANNED!");
        return user;
    }

    /**
     * Get the webshop inventory of gnomes.
     * 
     * @return a list of the inventory.
     */
    public List<Gnome> getGnomes() {
        Query query = em.createQuery("SELECT g FROM Gnome g");
        return query.getResultList();
    }

    /*************************
     * SHOPPING CART METHODS *
     *************************/
    
    /**
     * Adds a gnome and the quanity that is to be bought to a specifed users
     * shopping cart.
     *
     * @param currentUser the specified user.
     * @param gnome the gnome to add to the cart.
     * @param amount the quantity of gnomes of the specifed type.
     * @throws UserNotFoundException if the user is not found.
     */
    public void addToCart(ShopUser currentUser, Gnome gnome, int amount) throws UserNotFoundException {
        ShopUser u = em.find(ShopUser.class, currentUser.getUsername());
        if (u == null)
            throw new UserNotFoundException("User not found");
        u.addToCart(gnome, amount);
        em.persist(u);
    }
    
    /**
     * Gets the content of a specific users shopping cart.
     * 
     * @param currentUser the specified user.
     * @return the shopping cart.
     */
    public HashMap<Gnome,Integer> getCart(ShopUser currentUser) {
        ShopUser u = em.find(ShopUser.class, currentUser.getUsername());
        return u.getCart();
    }

    /**
     * Buys all the items in the specified users shopping cart.
     *
     * @param currentUser the specified user.
     * @throws GnomeNotFoundException if one of the gnomes in the shopping cart
     * does not exist in the inventory.
     * @throws GnomeAmountException if one shopping cart item is bought more
     * than there is availablity in the inventory.
     * @throws UserNotFoundException if the user is not found.
     */
    public void pay(ShopUser currentUser) throws GnomeNotFoundException, GnomeAmountException, UserNotFoundException {
        ShopUser u = em.find(ShopUser.class, currentUser.getUsername());
        if (u == null)
            throw new UserNotFoundException("User not found");
        Map<Gnome, Integer> cart = u.getCart();
        
        for (Gnome cartGnome : cart.keySet()) {
            Gnome inventoryGnome = em.find(Gnome.class, cartGnome.getName());
            if(inventoryGnome == null)
                throw new GnomeNotFoundException("The gnome " + cartGnome.getName() + " does not exist.");
            inventoryGnome.decrementAmount(cart.get(cartGnome));
            em.persist(inventoryGnome);
        }
        u.emptyCart();
        em.persist(u);
    }
    
    /*****************
     * ADMIN METHODS *
     *****************/
    
    /**
     * Login a admin.
     * 
     * @param username the admin username.
     * @param password the admin password.
     * @return the admin user account.
     * @throws UserLoginException if the user is not admin or if user
     * credentials are wrong.
     */
    public ShopUser loginAdmin(String username, String password) throws UserLoginException {
        ShopUser admin = login(username, password);
        if (!admin.getIsAdmin()) 
            throw new UserLoginException("You are not admin!");
        return admin;
    }
    
    /**
     * Add a gnome to the inventory.
     * 
     * @param name the name of the gnome.
     * @param amount the quanitity of the gnome.
     */
    public void addGnome(String name, int amount) {
        Gnome gnome = new Gnome(name, amount);
        em.persist(gnome);
    }

    /**
     * Remove a gnome from the inventory
     * 
     * @param selectedGnome the name of the gnome to remove.
     */
    public void removeGnome(String selectedGnome) {
        Query query = em.createQuery("DELETE FROM Gnome g WHERE g.name LIKE :gnomeName");
        query.setParameter("gnomeName", Gnome.parseName(selectedGnome));
        query.executeUpdate();
    }
    
    /**
     * Add to the quantity of a gnome.
     * 
     * @param selectedGnome the name of the gnome.
     * @param amount the amount to increment the quantity by.
     * @throws GnomeNotFoundException if the gnome is not found in the database.
     */
    public void addGnomesOfSameType(String selectedGnome, int amount) throws GnomeNotFoundException {
        Gnome g = em.find(Gnome.class, Gnome.parseName(selectedGnome));
        if (g == null)
            throw new GnomeNotFoundException("Gnome not found");
        g.incrementAmount(amount);
        em.persist(g);
    }

    /**
     * Gets all the users of the webshop.
     * 
     * @return a list of all the users.
     */
    public List<ShopUser> getUsers() {
        Query query = em.createQuery("SELECT u FROM ShopUser u");
        return query.getResultList();
    }

    /**
     * Bans a specific user from the webshop.
     * 
     * @param selectedUser the name of the user to ban.
     * @throws UserNotFoundException if the user is not found in the database.
     */
    public void banUser(String selectedUser) throws UserNotFoundException {
        ShopUser u = em.find(ShopUser.class, selectedUser);
        if (u == null)
            throw new UserNotFoundException("User not found");
        u.setIsBanned(true);
        em.persist(u);
    }
    
}
