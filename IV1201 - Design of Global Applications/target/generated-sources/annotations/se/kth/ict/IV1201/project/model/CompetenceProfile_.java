package se.kth.ict.IV1201.project.model;

import java.math.BigDecimal;
import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import se.kth.ict.IV1201.project.model.Competence;
import se.kth.ict.IV1201.project.model.Person;

@Generated(value="EclipseLink-2.5.2.v20140319-rNA", date="2016-03-10T14:37:26")
@StaticMetamodel(CompetenceProfile.class)
public class CompetenceProfile_ { 

    public static volatile SingularAttribute<CompetenceProfile, BigDecimal> yearsOfExperience;
    public static volatile SingularAttribute<CompetenceProfile, Competence> competenceId;
    public static volatile SingularAttribute<CompetenceProfile, Person> personId;
    public static volatile SingularAttribute<CompetenceProfile, Long> competenceProfileId;

}