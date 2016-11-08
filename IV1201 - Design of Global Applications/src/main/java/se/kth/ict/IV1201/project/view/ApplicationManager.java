package se.kth.ict.IV1201.project.view;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import javax.ejb.EJB;
import javax.el.ELContext;
import javax.enterprise.context.SessionScoped;
import javax.faces.context.FacesContext;
import javax.inject.Inject;
import javax.inject.Named;
import org.apache.log4j.Logger;
import se.kth.ict.IV1201.project.controller.Controller;
import se.kth.ict.IV1201.project.dto.AvailabilityDTO;
import se.kth.ict.IV1201.project.model.Availability;

/**
 * A backing bean for calls from the view concerning job application.
 */
@Named("applicationManager")
@SessionScoped
public class ApplicationManager implements Serializable {
    
    @EJB
    private Controller controller;
    
    @Inject private AvailabilityDTO newAvailability;
    
    private String selectedCompetence;
    
    private double yearsOfExperience;
    private final Collection<AvailabilityDTO> availableRanges = new ArrayList<>();
    private final Map<String, Double> hadCompetences = new HashMap<>();
    
    private static final Logger LOG = Logger.getLogger(ApplicationManager.class);
    
    /**
     * Gets the DTO for setting a new availability range.
     * 
     * @return the DTO.
     */
    public AvailabilityDTO getNewAvailability() {
        return newAvailability;
    }
    
    /**
     * Adds a availability range to the list of ranges.
     */
    public void addAvailablity() {
        availableRanges.add(new Availability(newAvailability));
    }
    
    /**
     * Gets all the added availability ranges.
     * 
     * @return the collection of the ranges.
     */
    public Collection<AvailabilityDTO> getAvailableRanges() {
        return availableRanges;
    }
    
    /**
     * Gets all the competences that are available to choose from.
     * 
     * @param locale the languages to get the competences in.
     * @return the collection of the competences.
     */
    public Collection<?> getCompetences(Locale locale) {
        return controller.getCompetences(locale);
    }
    
    /**
     * Sets the selected competence.
     * 
     * @param selectedCompetence the new selected competence.
     */
    public void setSelectedCompetence(String selectedCompetence) {
        this.selectedCompetence = selectedCompetence;
    }
    
    /**
     * Gets the selected competence.
     * 
     * @return the selected competence.
     */
    public String getSelectedCompetence() {
        return selectedCompetence;
    }
    
    /**
     * Sets the years of experience for a selected competence.
     * 
     * @param yearsOfExperience the number of years of experience.
     */
    public void setYearsOfExperience(double yearsOfExperience) {
        this.yearsOfExperience = yearsOfExperience;
    }
    
    /**
     * Gets the years of experience for a selected competence.
     * 
     * @return the number of years of experience.
     */
    public double getYearsOfExperience() {
        return yearsOfExperience;
    }
    
    /**
     * Add the selected competence with the set number of years of experience.
     */
    public void addCompetence() {
        hadCompetences.put(selectedCompetence, yearsOfExperience);
    }
    
    /**
     * Gets the competences that has been added.
     * 
     * @return the map of competences to years of experience in the competence.
     */
    public Map<String, Double> getHadCompetences() {
        return hadCompetences;
    }
    
    /**
     * Sends the application to the server to be added to the database.
     */
    public void addApplication() {
        ELContext elContext = FacesContext.getCurrentInstance().getELContext();
        UserManager um = (UserManager) FacesContext.getCurrentInstance().getApplication()
            .getELResolver().getValue(elContext, null, "userManager");
        
        controller.addApplication(availableRanges, hadCompetences, um.getCurrentUser());
    }
    
}
