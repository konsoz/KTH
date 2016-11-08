package se.kth.ict.IV1201.project.view;

import java.io.Serializable;
import javax.ejb.EJB;
import javax.el.ELContext;
import javax.enterprise.context.SessionScoped;
import javax.faces.context.FacesContext;
import javax.inject.Inject;
import javax.inject.Named;
import org.apache.commons.mail.EmailException;
import org.apache.log4j.Logger;
import se.kth.ict.IV1201.project.controller.Controller;
import se.kth.ict.IV1201.project.dto.RegistrationDTO;
import se.kth.ict.IV1201.project.dto.RestoreCredentialsDTO;
import se.kth.ict.IV1201.project.dto.UserDTO;
import se.kth.ict.IV1201.project.exceptions.LocalizedException;
import se.kth.ict.IV1201.project.exceptions.UserExistException;
import se.kth.ict.IV1201.project.exceptions.WrongEmailException;

/**
 * A backing bean for calls from the view concerning users of the recruitment
 * application.
 *
 * @author Konstantin Sozinov
 * @author Daniel Buchbergeer
 * @author Milan Stojanovic
 */
@Named("userManager")
@SessionScoped //One for each client
public class UserManager implements Serializable  {
    
    @EJB
    private Controller controller;
    
    @Inject private RegistrationDTO newPerson;
    @Inject private RestoreCredentialsDTO restore;
    
    private UserDTO currentUser;
    private Exception transactionFailure;
    private static final Logger log = Logger.getLogger(UserManager.class);
    
    /**
     * Registers a candidate on the recruitment application
     */
    public void register() {
        try {
            transactionFailure = null;
            controller.register(getNewPerson());
        } catch (Exception e) {
            handleException(e);
        }
        
    }   

    /**
     * Gets the DTO for registering a new user.
     * 
     * @return the DTO object.
     */
    public RegistrationDTO getNewPerson() {
        return newPerson;
    }
    
    /**
     * Gets the current user DTO containing information on the logged in user.
     * 
     * @return the DTO object.
     */
    public UserDTO getCurrentUser(){
        if (currentUser == null) {
            String username = FacesContext.getCurrentInstance().getExternalContext().getRemoteUser();
            if (username != null) {
                currentUser = controller.getUser(username);
            }
        }
        return currentUser;
    }
    
    /**
     * Checks if anyone is logged in.
     * 
     * @return <code>true</code> if there is a logged in user otherwise <code>
     * false</code>.
     */
    public boolean getIsLoggedIn() {
        return (getCurrentUser() != null);
    }
    
    /**
     * Handle any exception.
     * 
     * @param e the exception to handle.
     */
    private void handleException(Exception e) {
        e.printStackTrace(System.err);
        transactionFailure = e;
        log.error(e);
    }
    
    /**
     * Checks if the latest thrown exception is localized.
     * 
     * @return 
     */
    public boolean getIsExceptionLocalized() {
        return transactionFailure instanceof LocalizedException;
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
     * Gets the latest thrown exception in localized form.
     * 
     * @return the exception.
     */
    public LocalizedException getLocalizedException() {
        return (LocalizedException) transactionFailure;
    }
    
    /**
     * Gets if the latest transaction succeeds.
     * 
     * @return <code>true</code> if the latest transaction succeeded, otherwise
     * <code>false</code>.
     */
    public boolean getSuccess() {
        return transactionFailure == null;
    }
    
    /**
     * Logout any logged in user.
     * 
     * @return JSF reload string.
     */
    public String logoutUser() {
        currentUser = null;
        FacesContext.getCurrentInstance().getExternalContext().invalidateSession();
        return "login?faces-redirect=true";
    }
    
    /**
     * 
     * This method will generate new username and password for a
     * user who has forgot credentials.
     * 
     */
    public void restoreCredentials() {
        try {
            transactionFailure = null;
            controller.restoreCredentials(getRestore());
        } catch (WrongEmailException | EmailException e) {
            handleException(e);
        }
    }

    /**
     * @return the restore
     */
    public RestoreCredentialsDTO getRestore() {
        return restore;
    }   
    
}
