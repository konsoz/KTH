package se.kth.ict.IV1201.project.model;

import java.security.SecureRandom;
import java.util.logging.Level;
import org.apache.log4j.Logger;
import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang.RandomStringUtils;
import org.apache.commons.mail.DefaultAuthenticator;
import org.apache.commons.mail.Email;
import org.apache.commons.mail.EmailException;
import org.apache.commons.mail.SimpleEmail;
import se.kth.ict.IV1201.project.dto.UserDTO;
import se.kth.ict.IV1201.project.exceptions.UserExistException;
import se.kth.ict.IV1201.project.exceptions.WrongEmailException;
import se.kth.ict.IV1201.project.integration.RecruitmentDAO;

/**
 * This class encapsulates functionality for user logic, i.e. it contains
 * methods for user registration, login etc. It performs necessary logic on user
 * objects and/or calls the database.
 *
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 * @author Milan Stojanovic
 */
@TransactionAttribute(TransactionAttributeType.MANDATORY)
@Stateless
public class UserLogic {

    @EJB
    private RecruitmentDAO dao;

    private static final Logger log = Logger.getLogger(UserLogic.class);

    private static final String APPLICANT = "applicant";

    private String hashPassword(String password) {
        return DigestUtils.sha256Hex(password);
    }

    /**
     * Registers a candidate on the recruitment application
     *
     * @param p An person object(candidate) with personal information provided
     * in order to register a candidate.
     * @throws UserExistException if the username already is taken.
     */
    public void register(Person p) throws UserExistException {
        
        if (dao.findUser(p.getUsername()) != null) 
            throw new UserExistException(p.getUsername());
        
        Role r = new Role(APPLICANT, p.getUsername());
        dao.addRole(r);
        p.setRoleId(r);
        p.setPassword(hashPassword(p.getPassword()));
        dao.registerPerson(p);
        log.info("User " + p.getUsername() + " registered");
    }

    /**
     * Gets a user from a given username.
     *
     * @param username the username of the user to be found.
     * @return the found user.
     */
    public UserDTO getUser(String username) {
        return dao.findUser(username);
    }

    /**
     * This method will generate new username and password for a
     * user who has forgot credentials.
     * 
     * @param person who will reset credentials
     * @throws WrongEmailException
     * @throws org.apache.commons.mail.EmailException
     */
    public void restoreCredentials(Person person) throws WrongEmailException, EmailException {
        Person personEntity = dao.findByEmail(person.getEmail());
        if(personEntity == null) {
            throw new WrongEmailException(person.getEmail());
        }
        String email = personEntity.getEmail();
        String newPassword = RandomStringUtils.randomAlphanumeric(10).toUpperCase();
        String newUsername = RandomStringUtils.randomAlphanumeric(10).toUpperCase();
        
        while(dao.findUser(newUsername) != null) {
             newUsername = RandomStringUtils.randomAlphanumeric(10).toUpperCase();
        }
        personEntity.setPassword(DigestUtils.sha256Hex(newPassword));
        personEntity.setUsername(newUsername);
        Role role = personEntity.getRoleId();
        role.setUsername(newUsername);
        dao.addRole(role);
        dao.updateUser(personEntity);
        sendEmail(newPassword, newUsername, email);
    }
    
    /**
     * 
     * This method is evoked in order to send new credentials to the user.
     * 
     * @param newPassowrd New password 
     * @param newUsername New username
     * @param personEmail Persons email
     */
    public void sendEmail(String newPassowrd, String newUsername, String personEmail) throws EmailException {
        Email email = new SimpleEmail();
        email.setHostName("smtp.gmail.com");
        email.setSmtpPort(465);
        //email.setAuthentication("IV1201Recruitmentmanagement@gmail.com", "qaz123wsxedc");
        email.setAuthenticator(new DefaultAuthenticator("IV1201Recruitmentmanagement@gmail.com", "qaz123wsxedc"));
        // Username: IV1201Recruitmentmanagement
        // Password: qaz123wsxedc
        email.setStartTLSEnabled(true);
        email.setFrom("IV1201Recruitmentmanagement@gmail.com");
        email.setSubject("Credentials");
        StringBuilder str = new StringBuilder();
        str.append("Your new credentials: \n");
        str.append("Password: " + newPassowrd + "\n");
        str.append("Username: " + newUsername + "\n");
        email.setMsg(str.toString());
        email.addTo(personEmail);
        email.send();
    }
}
