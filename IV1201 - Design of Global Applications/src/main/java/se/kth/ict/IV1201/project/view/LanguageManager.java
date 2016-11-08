package se.kth.ict.IV1201.project.view;

import java.util.Locale;
import java.io.Serializable;
import javax.annotation.PostConstruct;
import javax.inject.Named;
import javax.enterprise.context.SessionScoped;
import javax.faces.context.FacesContext;

/**
 * This class provides internalization of the application. Presentation layer of
 * the application supports following languages: English, Swedish, Spanish.
 *
 * @author Daniel Buchberger
 */
@Named("localeManager")
@SessionScoped
public class LanguageManager implements Serializable {
    
    private static final long serialVersionUID = 1L;

    private Locale locale;

    /**
     * initialization of default language.
     */
    @PostConstruct
    public void init() {
        locale = FacesContext.getCurrentInstance().getExternalContext().getRequestLocale();
    }

    /**
     * Gets the current locale.
     * 
     * @return the locale.
     */
    public Locale getLocale() {
        return locale;
    }

    /**
     * Get current language.
     * 
     * @return the language in the form of a string.
     */
    public String getLanguage() {
        return locale.getLanguage();
    }

    /**
     * Set new current language.
     * 
     * @param language the language to change to.
     */
    public void setLanguage(String language) {
        locale = new Locale(language);
        FacesContext.getCurrentInstance().getViewRoot().setLocale(locale);
    }
} 