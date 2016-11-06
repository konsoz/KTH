package se.kth.id2212.ex4.bank.model;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

/**
 * Exchange entity containing relevant infromation for convering one currency to
 * another
 *
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 */
@Entity
public class Exchange implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private int id;
    private String fromCurrency;
    private String toCurrency;
    private double exchangeRate;
    
    public Exchange() {}
    
    public Exchange(String from, String to, double rate) {
        this.fromCurrency = from;
        this.toCurrency = to;
        this.exchangeRate = rate;
    }
    
    public double getRate() {
        return exchangeRate;
    }
    
    public String getFromCurrency() {
        return fromCurrency;
    }
    
    /**
     * Convert a value of the from currency to the coresponding value of the to
     * currency
     *
     * @param value the value to be converted
     * @return the result of the convertion
     */
    public double convert(double value) {
        return value * exchangeRate;
    }
    
}
