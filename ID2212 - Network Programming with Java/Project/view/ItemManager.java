package se.kth.id2212.ex4.shop.view;

import java.io.Serializable;
import java.util.List;
import javax.ejb.EJB;
import javax.enterprise.context.SessionScoped;
import javax.inject.Named;
import se.kth.id2212.ex4.shop.controller.ShopController;
import se.kth.id2212.ex4.shop.model.Gnome;
import se.kth.id2212.ex4.shop.model.GnomeNotFoundException;

/**
 * A manager for calls from the view concerning items in the inventory.
 */
@Named("itemManager")
@SessionScoped
public class ItemManager implements Serializable {
    
    private static final long serialVersionUID = 16247164405L;
    @EJB
    private ShopController shopController;
    private Exception transactionFailure;
    private String newGnomeName;
    private String newGnomeAmount;
    private String selectedGnome;
    private String nbrOfGnomesToAdd;
    
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
     * Add a gnome to inventory. 
     */
    public void addGnome() {
        try {
            transactionFailure = null;
            shopController.addGnome(newGnomeName, Integer.parseInt(newGnomeAmount));
        } catch (NumberFormatException e) {
            handleException(e);
        }
    }
    
    /**
     * Remove a gnome from the inventory.
     */
    public void removeGnome() {
        shopController.removeGnome(selectedGnome);
    }
    
    /**
     * Add quantity to a specific gnome.
     */
    public void addNbrOfGnomes() {
        try {
            transactionFailure = null;
            shopController.addGnomesOfSameType(selectedGnome, Integer.parseInt(nbrOfGnomesToAdd));
        } catch (NumberFormatException | GnomeNotFoundException e) {
            handleException(e);
        }
    }
    
    /**
     * Getters and setters 
     */

    /**
     * Gets the Gnomes from the inventory.
     * 
     * @return a list of the gnomes.
     */
    public List<Gnome> getGnomes() {
        return shopController.getGnomes();
    }
    
    public String getSelectedGnome() {
        return selectedGnome;
    }

    public void setSelectedGnome(String selectedGnome) {
        this.selectedGnome = selectedGnome;
    }

    public String getNewGnomeName() {
        return newGnomeName;
    }

    public void setNewGnomeName(String newGnomeName) {
        this.newGnomeName = newGnomeName;
    }

    public String getNewGnomeAmount() {
        return newGnomeAmount;
    }

    public void setNewGnomeAmount(String newGnomeAmount) {
        this.newGnomeAmount = newGnomeAmount;
    }

    public String getNbrOfGnomesToAdd() {
        return nbrOfGnomesToAdd;
    }

    public void setNbrOfGnomesToAdd(String nbrOfGnomesToAdd) {
        this.nbrOfGnomesToAdd = nbrOfGnomesToAdd;
    }

}
