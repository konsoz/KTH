package se.kth.ict.IV1201.project.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.CollectionAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import se.kth.ict.IV1201.project.model.Availability;
import se.kth.ict.IV1201.project.model.CompetenceProfile;
import se.kth.ict.IV1201.project.model.Role;

@Generated(value="EclipseLink-2.5.2.v20140319-rNA", date="2016-03-10T14:37:26")
@StaticMetamodel(Person.class)
public class Person_ { 

    public static volatile CollectionAttribute<Person, CompetenceProfile> competenceProfileCollection;
    public static volatile SingularAttribute<Person, String> password;
    public static volatile CollectionAttribute<Person, Availability> availabilityCollection;
    public static volatile SingularAttribute<Person, String> surname;
    public static volatile SingularAttribute<Person, Role> roleId;
    public static volatile SingularAttribute<Person, String> name;
    public static volatile SingularAttribute<Person, Long> personId;
    public static volatile SingularAttribute<Person, String> email;
    public static volatile SingularAttribute<Person, String> ssn;
    public static volatile SingularAttribute<Person, String> username;

}