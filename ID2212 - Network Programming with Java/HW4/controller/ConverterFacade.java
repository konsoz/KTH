package se.kth.id2212.ex4.bank.controller;

import java.util.ArrayList;
import java.util.List;
import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import javax.swing.JOptionPane;
import se.kth.id2212.ex4.bank.model.Exchange;

/**
 * A controller. All calls to the model that are executed from the view passes
 * through here.
 */
@TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
@Stateless
public class ConverterFacade {
    @PersistenceContext(unitName = "bankPU")
    private EntityManager em;

    /**
     * Converts a value from one currency to another.
     * 
     * @param from the currency to convert from.
     * @param to the currency to convert to.
     * @param value the value to convert.
     * @return the result of the convertion.
     */
    public double convertCurrency(String from, String to, double value) {
        Query query = em.createQuery(
            "SELECT e FROM Exchange e " + 
            "WHERE e.fromCurrency LIKE :from " + 
            "AND e.toCurrency LIKE :to");
        query.setParameter("from", from);
        query.setParameter("to", to);

        Exchange exchange = (Exchange) query.getSingleResult();
        return exchange.convert(value);
    }

    /**
     * Gets all available currencies
     * 
     * @return a list of all currencies.
     */
    public List<String> getCurrencies() {
        Query query = em.createQuery("SELECT DISTINCT(e.fromCurrency) FROM Exchange e");
        return query.getResultList();
    }
}
