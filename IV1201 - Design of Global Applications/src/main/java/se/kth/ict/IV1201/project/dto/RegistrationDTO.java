package se.kth.ict.IV1201.project.dto;

/**
 * This interface is used when a candidate registers to the application.
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 * @author Milan Stojanovic
 */
public interface RegistrationDTO {

    public void setName(String name);

    public void setSurname(String surname);

    public void setSsn(String ssn);

    public void setEmail(String email);

    public void setPassword(String password);

    public void setUsername(String username);

}
