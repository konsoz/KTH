package se.kth.ict.IV1201.project.model;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import org.apache.log4j.Logger;
import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.el.ELContext;
import javax.faces.context.FacesContext;
import se.kth.ict.IV1201.project.dto.AvailabilityDTO;
import se.kth.ict.IV1201.project.integration.RecruitmentDAO;
import se.kth.ict.IV1201.project.view.LanguageManager;

@TransactionAttribute(TransactionAttributeType.MANDATORY)
@Stateless
public class ApplicationLogic {

    @EJB
    private RecruitmentDAO dao;

    private static final Logger LOG = Logger.getLogger(ApplicationLogic.class);

    /**
     * Gets a collection of all available competences that the user can choose 
     * from for their application.
     * 
     * @param locale the languages to get the competences in.
     * @return the collection.
     */
    public Collection<String> getCompetences(Locale locale) {
        return dao.getCompetences(locale);
    }

    /**
     * Adds a application from a user of their available time and competence. 
     * 
     * @param availability a collection of date ranges.
     * @param competence a map of competences to years of each competence.
     * @param user the user who the application belongs to.
     */
    public void addApplication(Collection<AvailabilityDTO> availability, Map<String, Double> competence, Person user) {
        for (AvailabilityDTO availabilityDTO : availability) {
            Availability a = (Availability)availabilityDTO;
            a.setPersonId(user);
            dao.addAvailability(a);
        }
        for (Map.Entry<String, Double> entry : competence.entrySet()){
            Competence c = dao.findCompetence(entry.getKey());
            double yoe = entry.getValue();
            CompetenceProfile compProf = new CompetenceProfile(BigDecimal.valueOf(yoe), c, user);
            dao.addCompetenceProfile(compProf);
        }
    }
    
}
