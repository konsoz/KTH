package se.kth.ict.IV1201.project.model;

import java.io.Serializable;
import java.util.Collection;
import java.util.Locale;
import javax.enterprise.context.Dependent;
import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

/**
 * This entity represents a candidate competence, i.e. it encapsulates
 * information about which skills a candidate has. It contains columns name.
 * Primary key is competence_id.
 *
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 * @author Milan Stojanovic
 */
@Entity
@Table(name = "COMPETENCE")
@Dependent
@NamedQueries({
    @NamedQuery(name = "Competence.findAll", query = "SELECT c FROM Competence c"),
    @NamedQuery(name = "Competence.findByCompetenceId", query = "SELECT c FROM Competence c WHERE c.competenceId = :competenceId"),
    @NamedQuery(name = "Competence.findByName", query = "SELECT c FROM Competence c WHERE c.name = :name")})
public class Competence implements Serializable {

    private static final long serialVersionUID = 1L;
    @Id
    @Basic(optional = false)
    @NotNull
    @Column(name = "COMPETENCE_ID")
    private Long competenceId;
    @Size(max = 255)
    @Column(name = "NAME", unique = true)
    private String name;
    @OneToMany(mappedBy = "competenceId")
    private Collection<CompetenceProfile> competenceProfileCollection;
    @ManyToOne(fetch=FetchType.EAGER)
    @JoinColumn(name="NAME", insertable =  false, updatable = false)
    private Localization localization;
    
    public Competence() {
    }

    public Competence(Long competenceId) {
        this.competenceId = competenceId;
    }
       
    public Long getCompetenceId() {
        return competenceId;
    }
    
    public Localization getLocalization() {
        return localization;
    }
    
    public void setCompetenceId(Long competenceId) {
        this.competenceId = competenceId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Collection<CompetenceProfile> getCompetenceProfileCollection() {
        return competenceProfileCollection;
    }

    public void setCompetenceProfileCollection(Collection<CompetenceProfile> competenceProfileCollection) {
        this.competenceProfileCollection = competenceProfileCollection;
    }
    
    public String getLocalizedName(Locale locale) {
        return getLocalization().getTranslation(locale);
    }
}
