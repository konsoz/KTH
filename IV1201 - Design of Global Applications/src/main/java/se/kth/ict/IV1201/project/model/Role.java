package se.kth.ict.IV1201.project.model;

import java.io.Serializable;
import java.util.Collection;
import java.util.Locale;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
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

/**
 * This entity represents a role of a user in the application, a role can be a
 * candidate or a recruiter. It contains name. Primary key is role_id.
 *
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 * @author Milan Stojanovic
 */
@Entity
@Table(name = "ROLE")
@NamedQueries({
    @NamedQuery(name = "Role.findAll", query = "SELECT r FROM Role r"),
    @NamedQuery(name = "Role.findByRoleId", query = "SELECT r FROM Role r WHERE r.roleId = :roleId"),
    @NamedQuery(name = "Role.findByName", query = "SELECT r FROM Role r WHERE r.name LIKE :name"),
    @NamedQuery(name = "Role.findByUsername", query = "SELECT r FROM Role r WHERE r.username LIKE :username")})
public class Role implements Serializable {

    private static final long serialVersionUID = 1L;
    @Id @GeneratedValue(strategy=GenerationType.IDENTITY)
    @Column(name = "ROLE_ID")
    private long roleId;
    @Size(max = 255)
    @Column(name = "NAME")
    private String name;
    @Size(max = 255)
    @Column(name = "USERNAME")
    private String username;
    @OneToMany(mappedBy = "roleId")
    private Collection<Person> personCollection;
    @ManyToOne(fetch=FetchType.LAZY)
    @JoinColumn(name="NAME", insertable =  false, updatable = false)
    private Localization localization;

    public Role() {
    }

    public Role(String name, String username) {
        this.name = name;
        this.username = username;
    }

    public long getRoleId() {
        return roleId;
    }
    
    public Localization getLocalization() {
        return localization;
    }

    public void setRoleId(long roleId) {
        this.roleId = roleId;
    }

    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public String getLocalizedName(Locale locale) {
        return getLocalization().getTranslation(locale);
    }
    
    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public Collection<Person> getPersonCollection() {
        return personCollection;
    }

    public void setPersonCollection(Collection<Person> personCollection) {
        this.personCollection = personCollection;
    }
}
