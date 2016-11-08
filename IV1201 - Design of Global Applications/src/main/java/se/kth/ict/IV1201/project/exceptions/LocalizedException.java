/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package se.kth.ict.IV1201.project.exceptions;

import java.util.Locale;
import java.util.ResourceBundle;

/**
 * Super class for all exceptions that are localized. It defines a method 
 * getLocalizedMessage that takes a locale and gets the localized messages from
 * the resource bundle. 
 */
public class LocalizedException extends Exception {

    /**
     * Constructs an instance of <code>InternationalizedException</code> with
     * the specified detail message.
     *
     * @param msg the detail message.
     */
    public LocalizedException(String msg) {
        super(msg);
    }
    
    /**
     * Gets the localized message from the resource bundle based on a provided 
     * locale.
     * 
     * @param locale the locale that defines the language.
     * @return the localized string.
     */
    public String getLocalizedMessage(Locale locale) {
        ResourceBundle msgs = ResourceBundle.getBundle("i18n.language", locale);
        return msgs.getString(super.getMessage());
    }
}
