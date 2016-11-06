package se.kth.id2212.ex4.bank.view;

import se.kth.id2212.ex4.bank.controller.ConverterFacade;
import java.io.Serializable;
import java.util.List;
import javax.ejb.EJB;
import javax.enterprise.context.Conversation;
import javax.enterprise.context.ConversationScoped;
import javax.inject.Inject;
import javax.inject.Named;





@Named("converterManager")
@ConversationScoped
public class ConverterManager implements Serializable {

    private static final long serialVersionUID = 16247164405L;
    @EJB
    private ConverterFacade converterFacade;
    private String fromCurrencyName;
    private String toCurrencyName;
    private double fromCurrencyValue;
    private double toCurrencyValue;
    
    @Inject
    private Conversation conversation;

    private void startConversation() {
        if (conversation.isTransient()) {
            conversation.begin();
        }
    }

    private void stopConversation() {
        if (!conversation.isTransient()) {
            conversation.end();
        }
    }
    
    public void setFromCurrencyName(String fromCurrencyName) {
        this.fromCurrencyName = fromCurrencyName;
    }
    
    public String getFromCurrencyName(){
        return fromCurrencyName;
    }
    
    public void setToCurrencyName(String toCurrencyName) {
        this.toCurrencyName = toCurrencyName;
    }
    
    public String getToCurrencyName(){
        return toCurrencyName;
    }
    
    public void setFromCurrencyValue(double fromCurrencyValue) {
        this.fromCurrencyValue = fromCurrencyValue;
    }
    
    public double getFromCurrencyValue() {
        return fromCurrencyValue;
    }
    
    public void setToCurrencyValue(double toCurrencyValue) {
        this.toCurrencyValue = toCurrencyValue;
    }
    
    public double getToCurrencyValue(){
        return toCurrencyValue;
    }
    
    public List<String> getCurrencies() {
        startConversation();
        return converterFacade.getCurrencies();
    }
    
    /**
     * Performes the convertion of the currency.
     */
    public void convertCurrency() {
        startConversation();
        double test = converterFacade.convertCurrency(fromCurrencyName, toCurrencyName, fromCurrencyValue);
        System.out.println(test);
        toCurrencyValue = test;
    }
}