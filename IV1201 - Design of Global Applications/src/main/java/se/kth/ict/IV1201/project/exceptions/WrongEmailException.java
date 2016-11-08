
package se.kth.ict.IV1201.project.exceptions;

import java.text.MessageFormat;
import java.util.Locale;

/**
 * Is raised when a user with given email does not exist.
 * 
 * @author konstantin
 */
public class WrongEmailException extends LocalizedException {
    
    private final String email;
    
    public WrongEmailException(String msg) {
        super("exception_email_does_not_exist");
        this.email = msg;
    }

    @Override
    public String getLocalizedMessage(Locale locale) {
        String msg = super.getLocalizedMessage(locale);
        
        return MessageFormat.format(msg, email);
    }
}
