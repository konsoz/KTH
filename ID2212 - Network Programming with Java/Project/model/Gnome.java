package se.kth.id2212.ex4.shop.model;

import java.io.Serializable;
import java.util.Objects;
import javax.persistence.Entity;
import javax.persistence.Id;

/**
 * Persistent entity representation of a gnome in the inventory.
 */
@Entity
public class Gnome implements Serializable {
    
    @Id
    private String name;
    private int amount;
    
    /**
     * Creates a new instance of Gnome.
     */
    public Gnome() {}
    
    /**
     * Creates a new instance of Gnome with the suplied arguments.
     * 
     * @param name the name of the gnome.
     * @param amount the quanitity of the gnome type in the inventory.
     */
    public Gnome(String name, int amount) {
        this.name = name;
        this.amount = amount;
    }
    
    /**
     * Gets the name of the gnome.
     * 
     * @return the name.
     */
    public String getName() {
        return name;
    }
    
    /**
     * Gets the quantity of the gnome in the inventory.
     * 
     * @return the quantity.
     */
    public int getAmount() {
        return amount;
    }
    
    /**
     * Increases the quantity of this gnome.
     * 
     * @param amount the amount to increase.
     */
    public void incrementAmount(int amount) {
        this.amount = this.amount + amount;
    }
    
    /**
     * Decreases the quantity of this gnome.
     * 
     * @param amount the amount to decrease.
     * @throws GnomeAmountException if the resulting quantity is negative.
     */
    public void decrementAmount(int amount) throws GnomeAmountException {
        if (amount > this.amount)
            throw new GnomeAmountException("Not enough gnomes in stock");
        this.amount = this.amount - amount;
    }
    
    /**
     * The string representation of this object.
     * 
     * @return the string representation.
     */
    @Override
    public String toString() {
        return name + " - " + amount + "pcs";
    }

    /**
     * Given the string repesentation of this object returns the name part.
     * 
     * @param selectedGnome the string representation.
     * @return the name.
     */
    public static String parseName(String selectedGnome) {
        String[] parts = selectedGnome.split("\040-\040");
        return parts[0];
    }
    
    /**
     * Generates the ubject hash code.
     * 
     * @return the hash code.
     */
    @Override
    public int hashCode() {
        int hash = 7;
        hash = 13 * hash + Objects.hashCode(this.name);
        return hash;
    }

    /**
     * Compares this object to another.
     * 
     * @param that the other object
     * @return <code>true</code> is the objects are equal and <code>false</code>
     * if they are not.
     */
    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        }
        if (that == null) {
            return false;
        }
        if (getClass() != that.getClass()) {
            return false;
        }
        final Gnome other = (Gnome) that;
        if (!this.name.equals(other.name)) {
            return false;
        }
        return true;
    }
    
}
