package se.kth.ict.IV1201.project.model;

import java.io.Serializable;
import java.util.Collection;
import javax.enterprise.context.Dependent;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import se.kth.ict.IV1201.project.validation.ValidEmail;
import se.kth.ict.IV1201.project.validation.ValidName;
import se.kth.ict.IV1201.project.validation.ValidSSN;
import se.kth.ict.IV1201.project.dto.RegistrationDTO;
import se.kth.ict.IV1201.project.dto.RestoreCredentialsDTO;
import se.kth.ict.IV1201.project.dto.UserDTO;
import se.kth.ict.IV1201.project.validation.ValidUsername;
import se.kth.ict.IV1201.project.validation.ValidPassword;
import se.kth.ict.IV1201.project.validation.ValidSurname;

/**
 * This entity represents a user of the application, a user can be a candidate
 * or a recruiter. It contains columns name, surname, ssn, email, password,
 * username. Primary key is person_id. Foreign key is role_id.
 *
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 * @author Milan Stojanovic
 */
@Entity
@Table(name = "PERSON")
@Dependent
@NamedQueries({
    @NamedQuery(name = "Person.findAll", query = "SELECT p FROM Person p"),
    @NamedQuery(name = "Person.findByPersonId", query = "SELECT p FROM Person p WHERE p.personId = :personId"),
    @NamedQuery(name = "Person.findByName", query = "SELECT p FROM Person p WHERE p.name = :name"),
    @NamedQuery(name = "Person.findBySurname", query = "SELECT p FROM Person p WHERE p.surname = :surname"),
    @NamedQuery(name = "Person.findBySsn", query = "SELECT p FROM Person p WHERE p.ssn = :ssn"),
    @NamedQuery(name = "Person.findByEmail", query = "SELECT p FROM Person p WHERE p.email = :email"),
    @NamedQuery(name = "Person.findByPassword", query = "SELECT p FROM Person p WHERE p.password = :password"),
    @NamedQuery(name = "Person.findByUsername", query = "SELECT p FROM Person p WHERE p.username = :username"),
    @NamedQuery(name = "Person.findUser", query = "SELECT p FROM Person p WHERE p.username = :username AND p.password = :password")})
public class Person implements Serializable, RegistrationDTO, UserDTO, RestoreCredentialsDTO {

    private static final long serialVersionUID = 1L;
    @Id @GeneratedValue(strategy=GenerationType.IDENTITY)
    @Column(name = "PERSON_ID")
    private Long personId;
    @Size(max = 255)
    @Column(name = "NAME", nullable = false)
    @ValidName
    private String name;
    @Size(max = 255)
    @Column(name = "SURNAME", nullable = false)
    @ValidSurname
    private String surname;
    @Size(max = 255)
    @Column(name = "SSN", nullable = true)
    @ValidSSN
    private String ssn;
    @Size(max = 255)
    @Column(name = "EMAIL", unique = true, nullable = true)
    @ValidEmail
    private String email;
    @Size(max = 255)
    @Column(name = "PASSWORD", nullable = true)
    @ValidPassword
    private String password;
    @Size(max = 255)
    @Column(name = "USERNAME", unique = true, nullable = true)
    @ValidUsername
    private String username;
    @JoinColumn(name = "ROLE_ID", referencedColumnName = "ROLE_ID")
    @ManyToOne
    private Role roleId;
    @OneToMany(mappedBy = "personId")
    private Collection<Availability> availabilityCollection;
    @OneToMany(mappedBy = "personId")
    private Collection<CompetenceProfile> competenceProfileCollection;

    public Person() {
    }
    
    public Person(String name, String surname, String ssn, String email, String password, String username, Role roleId) {
        this.name = name;
        this.email = email;
        this.password = password;
        this.roleId = roleId;
        this.ssn = ssn;
        this.surname = surname;
        this.username = username;
    }

    public long getPersonId() {
        return personId;
    }

    public void setPersonId(long personId) {
        this.personId = personId;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String getSurname() {
        return surname;
    }

    @Override
    public void setSurname(String surname) {
        this.surname = surname;
    }

    public String getSsn() {
        return ssn;
    }

    @Override
    public void setSsn(String ssn) {
        this.ssn = ssn;
    }

    @Override
    public String getEmail() {
        return email;
    }

    @Override
    public void setEmail(String email) {
        this.email = email;
    }

    public String getPassword() {
        return password;
    }

    @Override
    public void setPassword(String password) {
        this.password = password;
    }

    @Override
    public String getUsername() {
        return username;
    }

    @Override
    public void setUsername(String username) {
        this.username = username;
    }

    @Override
    public Role getRoleId() {
        return roleId;
    }

    public void setRoleId(Role roleId) {
        this.roleId = roleId;
    }

    public Collection<Availability> getAvailabilityCollection() {
        return availabilityCollection;
    }

    public void setAvailabilityCollection(Collection<Availability> availabilityCollection) {
        this.availabilityCollection = availabilityCollection;
    }

    public Collection<CompetenceProfile> getCompetenceProfileCollection() {
        return competenceProfileCollection;
    }

    public void setCompetenceProfileCollection(Collection<CompetenceProfile> competenceProfileCollection) {
        this.competenceProfileCollection = competenceProfileCollection;
    }
}
