package se.kth.ict.IV1201.project.exceptions;

import java.text.MessageFormat;
import java.util.Locale;

/**
 * Thrown when a user with a certain username already exist.
 * 
 * @author milan
 */
public class UserExistException extends LocalizedException {

    private final String username;
    
    public UserExistException(String msg) {
        super("exception_username_already_exist");
        this.username = msg;
    }

    @Override
    public String getLocalizedMessage(Locale locale) {
        String msg = super.getLocalizedMessage(locale); 
        
        return MessageFormat.format(msg, username);
    }
}
