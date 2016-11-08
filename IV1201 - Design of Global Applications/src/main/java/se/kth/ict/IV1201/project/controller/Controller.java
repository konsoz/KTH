package se.kth.ict.IV1201.project.controller;

import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.annotation.security.DeclareRoles;
import javax.annotation.security.RolesAllowed;
import org.apache.commons.mail.EmailException;
import org.apache.log4j.Logger;
import se.kth.ict.IV1201.project.dto.AvailabilityDTO;
import se.kth.ict.IV1201.project.model.Person;
import se.kth.ict.IV1201.project.model.UserLogic;
import se.kth.ict.IV1201.project.dto.RegistrationDTO;
import se.kth.ict.IV1201.project.dto.RestoreCredentialsDTO;
import se.kth.ict.IV1201.project.dto.UserDTO;
import se.kth.ict.IV1201.project.exceptions.UserExistException;
import se.kth.ict.IV1201.project.exceptions.WrongEmailException;
import se.kth.ict.IV1201.project.model.ApplicationLogic;

/**
 * Controller of the application, redirects calls from presentation layer to the
 * model layer.
 *
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 * @author Milan Stojanovic
 */
@TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
@Stateless
@DeclareRoles({"applicant", "recruit"})
public class Controller {
    
    @EJB private UserLogic userLogic;
    @EJB private ApplicationLogic applicationLogic;
    
    private static final Logger LOG = Logger.getLogger(Controller.class);
    
    /**
     * Registers a candidate on the recruitment application
     *
     * @param p An interface with personal information provided in order to
     * register a candidate.
     * @throws UserExistException if the username already is taken.
     */
    public void register(RegistrationDTO p) throws UserExistException {
        userLogic.register((Person) p);
    }

    /**
     * Gets a user object with non-sensitive properties based on a username.
     *
     * @param username the searched for users username.
     * @return the user object containing the properties.
     */
    public UserDTO getUser(String username) {
        return userLogic.getUser(username);
    }

    /**
     * This method will generate new username and password for a
     * user who has forgot credentials.
     * 
     * @param restore contains information on the user who want to restore their
     * credentials.
     * @throws WrongEmailException
     */
    public void restoreCredentials(RestoreCredentialsDTO restore) throws WrongEmailException, EmailException {
        userLogic.restoreCredentials((Person) restore);
    }

    /**
     * Gets a collection of all available competences that the user can choose 
     * from for their application.
     * 
     * @param locale the languages to get the competences in.
     * @return the collection.
     */
    @RolesAllowed("applicant")
    public Collection<?> getCompetences(Locale locale) {
        return applicationLogic.getCompetences(locale);
    }

    /**
     * Adds a application from a user of their available time and competence. 
     * 
     * @param availability a collection of date ranges.
     * @param competence a map of competences to years of each competence.
     * @param user the user who the application belongs to.
     */
    public void addApplication(Collection<AvailabilityDTO> availability, Map<String, Double> competence, UserDTO user) {
        applicationLogic.addApplication(availability, competence, (Person) user);
    }
}
