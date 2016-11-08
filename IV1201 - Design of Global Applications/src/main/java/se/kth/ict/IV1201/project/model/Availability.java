package se.kth.ict.IV1201.project.model;

import java.io.Serializable;
import java.text.Format;
import java.text.SimpleDateFormat;
import java.util.Date;
import javax.enterprise.context.Dependent;
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
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.validation.constraints.NotNull;
import se.kth.ict.IV1201.project.dto.AvailabilityDTO;
import se.kth.ict.IV1201.project.validation.ValidDate;

/**
 * This entity represents a candidate availability, i.e. it encapsulates
 * information about when a candidate can work. It contains columns from_date,
 * to_date. Primary key is availability_id. person_id is a reference to person
 * entity.
 *
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 * @author Milan Stojanovic
 */
@Entity
@Table(name = "AVAILABILITY")
@Dependent
@NamedQueries({
    @NamedQuery(name = "Availability.findAll", query = "SELECT a FROM Availability a"),
    @NamedQuery(name = "Availability.findByAvailabilityId", query = "SELECT a FROM Availability a WHERE a.availabilityId = :availabilityId"),
    @NamedQuery(name = "Availability.findByFromDate", query = "SELECT a FROM Availability a WHERE a.fromDate = :fromDate"),
    @NamedQuery(name = "Availability.findByToDate", query = "SELECT a FROM Availability a WHERE a.toDate = :toDate")})
public class Availability implements Serializable, AvailabilityDTO {

    private static final long serialVersionUID = 1L;
    @Id @GeneratedValue(strategy=GenerationType.IDENTITY)
    @Basic(optional = false)
    @NotNull
    @Column(name = "AVAILABILITY_ID")
    private Long availabilityId;
    @Column(name = "FROM_DATE")
    @ValidDate
    @Temporal(TemporalType.DATE)
    private Date fromDate;
    @Column(name = "TO_DATE")
    @ValidDate
    @Temporal(TemporalType.DATE)
    private Date toDate;
    @JoinColumn(name = "PERSON_ID", referencedColumnName = "PERSON_ID")
    @ManyToOne
    private Person personId;

    public Availability() {
    }

    public Availability(Long availabilityId) {
        this.availabilityId = availabilityId;
    }

    public Availability(AvailabilityDTO that) {
        Availability availablity = (Availability)that;
        this.fromDate = availablity.fromDate;
        this.toDate = availablity.toDate;
    }

    public Long getAvailabilityId() {
        return availabilityId;
    }
    
    public void setAvailabilityId(Long availabilityId) {
        this.availabilityId = availabilityId;
    }
    
    public Date getFromDate() {
        return fromDate;
    }
    
    @Override
    public void setFromDate(Date fromDate) {
        this.fromDate = fromDate;
    }

    public Date getToDate() {
        return toDate;
    }

    @Override
    public void setToDate(Date toDate) {
        this.toDate = toDate;
    }

    public Person getPersonId() {
        return personId;
    }

    public void setPersonId(Person personId) {
        this.personId = personId;
    }   
    
    @Override
    public String toString() {
        Format formatter = new SimpleDateFormat("dd-MM-yyyy");
        String from = formatter.format(fromDate);
        String to = formatter.format(toDate);
        return from + " - " + to;
    }
    
}
