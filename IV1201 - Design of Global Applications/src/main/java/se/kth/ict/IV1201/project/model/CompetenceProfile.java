package se.kth.ict.IV1201.project.model;

import java.io.Serializable;
import java.math.BigDecimal;
import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * This entity represents a candidate competence profile, i.e. it encapsulates
 * information about how many years of experience a candidate has within one
 * competence. It contains columns years_of_experience. Primary key is
 * competence_profile_id. Foreign key is person_id.
 *
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 * @author Milan Stojanovic
 */
@Entity
@Table(name = "COMPETENCE_PROFILE")
@NamedQueries({
    @NamedQuery(name = "CompetenceProfile.findAll", query = "SELECT c FROM CompetenceProfile c"),
    @NamedQuery(name = "CompetenceProfile.findByCompetenceProfileId", query = "SELECT c FROM CompetenceProfile c WHERE c.competenceProfileId = :competenceProfileId"),
    @NamedQuery(name = "CompetenceProfile.findByYearsOfExperience", query = "SELECT c FROM CompetenceProfile c WHERE c.yearsOfExperience = :yearsOfExperience")})
public class CompetenceProfile implements Serializable {

    private static final long serialVersionUID = 1L;
    @Id @GeneratedValue(strategy=GenerationType.IDENTITY)
    @Basic(optional = false)
    @NotNull
    @Column(name = "COMPETENCE_PROFILE_ID")
    private Long competenceProfileId;
    @Column(name = "YEARS_OF_EXPERIENCE")
    private BigDecimal yearsOfExperience;
    @JoinColumn(name = "COMPETENCE_ID", referencedColumnName = "COMPETENCE_ID")
    @ManyToOne
    private Competence competenceId;
    @JoinColumn(name = "PERSON_ID", referencedColumnName = "PERSON_ID")
    @ManyToOne
    private Person personId;

    public CompetenceProfile() {
    }
    
    public CompetenceProfile(BigDecimal yearsOfExperience, Competence competenceId, Person personId) {
        this.yearsOfExperience = yearsOfExperience;
        this.competenceId = competenceId;
        this.personId = personId;
    }

    public CompetenceProfile(Long competenceProfileId) {
        this.competenceProfileId = competenceProfileId;
    }
  
    public Long getCompetenceProfileId() {
        return competenceProfileId;
    }

    public void setCompetenceProfileId(Long competenceProfileId) {
        this.competenceProfileId = competenceProfileId;
    }
    
    public BigDecimal getYearsOfExperience() {
        return yearsOfExperience;
    }
    
    public void setYearsOfExperience(BigDecimal yearsOfExperience) {
        this.yearsOfExperience = yearsOfExperience;
    }
    
    public Competence getCompetenceId() {
        return competenceId;
    }
    
    public void setCompetenceId(Competence competenceId) {
        this.competenceId = competenceId;
    }
    
    public Person getPersonId() {
        return personId;
    }
 
    public void setPersonId(Person personId) {
        this.personId = personId;
    }
}
