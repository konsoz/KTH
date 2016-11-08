package se.kth.ict.IV1201.project.integration;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;
import se.kth.ict.IV1201.project.model.Role;
import se.kth.ict.IV1201.project.model.Person;
import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import se.kth.ict.IV1201.project.model.Availability;
import se.kth.ict.IV1201.project.model.Competence;
import se.kth.ict.IV1201.project.model.CompetenceProfile;
import se.kth.ict.IV1201.project.model.Localization;

/**
 * The responsibility of this class is to handle database calls. It has no
 * dependencies on the business layer and does not contains any business logic.
 *
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 * @author Milan Stojanovic
 */
@TransactionAttribute(TransactionAttributeType.MANDATORY)
@Stateless
public class RecruitmentDAO {
    
    @PersistenceContext(unitName = "com.fluffyumbrella_IV1201_Project_war_1.0PU")
    private EntityManager em;
    
    /**
     * This method persists (registers) a candidate to the database.
     * 
     * @param person A candidate
     */
    public void registerPerson(Person person) {
        em.persist(person);
    }
    
    
    public void updateUser(Person person) {
        em.merge(person);
    }
    
    /**
     * This method finds a user from the database with the given username.
     *
     * @param username the username to search by.
     * @return the person with the given username.
     */
    public Person findUser(String username) {
        Query query = em.createNamedQuery("Person.findByUsername");
        query.setParameter("username", username);
        try {
            return (Person)query.getSingleResult();
        } catch (NoResultException e) {
            return null;
        }
    }
    
    /**
     * Persists a new role to the database.
     * 
     * @param r the new role to persist. 
     */
    public void addRole(Role r) {
        em.persist(r);
    }
 
    /**
     * This method will find a user with an email.
     * 
     * @param email email string
     * @return a person entity
     */
    public Person findByEmail(String email) {
        try {
          Query query = em.createNamedQuery("Person.findByEmail");
          query.setParameter("email", email); 
          return (Person)query.getSingleResult();
        } catch(NoResultException e) {
          return null;      
        }    
    }

    /**
     * Gets all available competences from the database based on a locale.
     * 
     * @param locale the languages to get the competences in.
     * @return a collection of the competences.
     */
    public Collection<String> getCompetences(Locale locale) {
        Query query = em.createNamedQuery("Competence.findAll");
        Collection<String> competences = new ArrayList<>();
        for(Competence c : (Collection<Competence>)query.getResultList())
            competences.add(c.getLocalizedName(locale));
        
        return competences;
    }
    
    /**
     * Persists a new availability to the database.
     * 
     * @param a the new availability to persist. 
     */
    public void addAvailability(Availability a) {
        em.persist(a);
    }
    
    /**
     * This method will find a competence based on a name.
     * 
     * @param name competence name.
     * @return a competence entity.
     */
    public Competence findCompetence(String name) {
        Query query = em.createNamedQuery("Competence.findByName");
        query.setParameter("name", name);
        try {
            return (Competence)query.getSingleResult();
        } catch (NoResultException e) {
            return null;
        }
    }
    
    /**
     * Persists a new competence profile to the database.
     * 
     * @param compProf the new competence profile to persist. 
     */
    public void addCompetenceProfile(CompetenceProfile compProf) {
        em.persist(compProf);
    }
}
