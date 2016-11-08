package se.kth.ict.IV1201.project.dto;

import se.kth.ict.IV1201.project.model.Role;

/**
 * This interface is used when a user is logged in to the application. It
 * provides access to the necessary user information.
 *
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 * @author Milan Stojanovic
 */
public interface UserDTO {
    
    public String getName();

    public String getSurname();

    public String getEmail();

    public String getUsername();

    public Role getRoleId();

}
